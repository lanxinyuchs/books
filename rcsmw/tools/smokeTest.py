#!/usr/bin/env python
# Eyinzho   
# 
# V0.1      fetch the latest UP from the mia, test waf build rcsmw
#           04/27/2016
#           
import sys
import os
import re 
import subprocess
import time
sys.path.append('/home/eyinzho/linux/lib/python2.7/site-packages/')
import pexpect 

class smokeTest:
   def __init__(self, up):
      self.prompt = '~#'
      self.wd = self.setWorkDir()
      self.up = self.getUp(up)
      self.mwVer = self.setupEnv(up)
      return

   def setWorkDir(self):
      toolD = os.path.realpath(__file__)
      wd = os.path.dirname(toolD)[:-5] + 'out'
      os.chdir(wd)
      print 'Change work dir to ', wd
      return wd

   def getUp(self, up):
      if up == 'd':
         print 'Get DUMMY UP from clearcase'
         path = 'https://rbs-rde.rnd.ki.sw.ericsson.se/vobs/rcs/delivery/VRCS_CSX10179_2/VRCS-UP_CXS101657_2/doc/19010/VRCS-UP_CXS101657_2.cxs'
      else:
         path = 'https://arm010-eiffel001.seki.rnd.ericsson.se:8443/nexus/content/repositories/public/com/ericsson/CXP9029198_1/%s/CXP9029198_1-%s.zip' % (up, up)
         print 'Get TEST UP from mia' 
         print '\n================================\n'
    
      fn = path.split('/')[-1] 
      p = os.popen('wget ' + path)
      for line in p.readlines():
         print line

      return fn

   def setupEnv(self, up):
      # ger VRCS MW REV
      mwVer = ''
      p = os.popen('ls VRCS-MW*.cxp')
      for line in p.readlines():
         if 'cxp' in line:
            mwVer = line[21:-5]
            print mwVer 

      if mwVer == '':
         print 'NO RCS-MW up found, please run tools/delivery.sh'
         sys.exit(1)

      if up == 'd':
         print 'Setup my UP for DUMMY LM TEST'
         p = os.popen('tar xf ' + self.up)
         for line in p.readlines():
            print line[:-1]
         uf = "cxs101657_2-up.xml"
      else:
         print 'Setup my UP for UP', up, 'TEST'
         p = os.popen('unzip ' + self.up)
         for line in p.readlines():
            print line[:-1]
         uf = "CXP9029198_1-up.xml"

      self.updateUpFile(uf, mwVer)

      # clean up
      os.remove(self.up)   
      os.remove('VRCS-MW_CXP9029176_2.cxp')
      
      return mwVer

   def updateUpFile(self, uf, mwVer):
      # update the up file
      fin  = open(uf)
      fout = open("tmp", "w")
      newstr = 'version="'+mwVer+'" filename="VRCS-MW_'
      for line in fin:
         fout.write(re.sub(r'version="(.*)" filename="VRCS-MW_', newstr, line))
      fin.close()
      fout.close()
      os.remove(uf)
      os.popen("mv tmp "+uf)
      return

   def genImage(self):
      print "Generating image file ......"
      name = 'mwtest-'+self.mwVer
      cmd = '/app/rcs-ee/vrcs/tools/rcs-ee_make_vm.sh -i -U ./'.split()
      self.runCmd(cmd)

      # generate cloud image
      cmd = 'glance image-create --name '+ name +\
            ' --file vrcs_hda_installed.qcow2 --disk-format qcow2 --container-format bare'
      print cmd
      p = os.popen(cmd)
      for line in p.readlines():
         print line[:-1]
      
      # boot instance from the image
      cmd = 'nova boot --nic net-name=KI10_cpp_pran --image ' + name +\
            ' --flavor VRCS --key-name vgkey ' + name
      print cmd      
      p = os.popen(cmd)
      for line in p.readlines():
         print line[:-1]
      
      # get the ip
      cmd = 'nova show ' + name
      print cmd
      ready = False
      host = ''
      while True:
         time.sleep(10)
         p = os.popen(cmd)
         for line in p.readlines():
            hostObj = re.search(r'(\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3})', line)
            if hostObj:
               host = hostObj.group(1)
               print 'Host ip is', host
            if 'ACTIVE' in line:
               ready = True
               print line
         if ready:
            break
      
      if host == '':
         print "Failed to get host ip"
         sys.exit(1)
      return host

   def runCmd(self, cmd):
      p = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
      out, err = p.communicate()
      print out
      if err:
         print err
         sys.exit(1)
      return   


   def checkLog(self, host):
      print 'Conect to ', host
      child = conn("ssh -o 'IdentitiesOnly yes' root@"+host)
     
      # check if system is up runing
      while True:
         if system_ready(child) or system_crash(child):
            if checkError(child):
               print '====SmokeTest FAIL!!!===='
            else:
               print '====SmokeTest PASS===='
            break
         else:
            print 'Node is still starting ...'
            time.sleep(10)
   
      discon(child)
      return

def system_ready(child):
   prompt = '~#'
   child.sendline('grep "Set status indicator to steady_on" /rcs/erlang/erlang.log.1')
   child.expect(prompt)
   res = child.before
   #print res, child.after
   if not "steady_on" in res:
      print "system not ready yet"
      return False
   return True
                                                  
def system_crash(child):
   prompt = '~#'
   child.sendline('grep "Erlang is crashing" /rcs/erlang/erlang.log.1')
   child.expect(prompt)
   res = child.before
   print res, child.after
   if "crashing" in res:
      print "Erlang is crashes"
      return True
   return False

def checkError(child):
   prompt = '~#'
   error = False
   # check erlang log
   child.sendline('grep -A30 -E "ERROR|CRASH" /rcs/erlang/erlang.log.1')
   child.expect(prompt)
   res = child.before
   if "ERROR REPORT" in res or "CRASH REPORT" in res:
      error = True
      print res

   # check te log
   child.sendline('te log read |grep -E "ERROR|EXCEPTION"')
   child.expect(prompt)
   res = child.before
   if "ERROR:" in res or "EXCEPTION:" in res:
      error = True
      print res

   # get pmd file
   child.sendline('ls /var/pmd/')
   child.expect(prompt)
   res = child.before
   if ".pmd" in res:
      error = True
      print child.after

   # check syslog
   return error
      
def conn(connStr):
   prompt = '~#'
   print connStr
   child = pexpect.spawn(connStr)
   i = child.expect(['password:', '(yes/no)?'] , timeout=120)
   print i
   print child.before, child.after
   if i==0:
      child.sendline('root')
      print 'login'
   elif i==1:
      print 'first connect to this address'
      child.sendline('yes')
      child.expect('password:', timeout=120)
      print child.before, child.after
      
      child.sendline('root')
      child.expect(prompt, timeout=20)
      print child.before, child.after

   return child

# disconnect
def discon(child):
   child.sendline('exit')
   child.close()

def main(argv):
   print '''
Please run the commands below to run the smoke test:
   ======================================
module add qemu/2.4.1
module add netcat
setenv PATH $PATH":/app/rbs/wrtools/tools-sdk-20150518/usr/sbin"
source /proj/rbs-g2-ci/5G/openstack/cpp-openrc.csh
module load /app/rbs/modules/python/2.7.11
'''
   
   usage = '''
Usage: tools/smokeTest.py -d
       tools/smokeTest.py -f <revision>
       d test only DUMMY package, f test latest package from mia website
'''
   if len(argv) == 0:
      print usage           
      return

   up = argv[0]

   if up == '-d' and len(argv) == 1:
      up = 'd'
   elif up == '-f' and len(argv) == 2:
      up = argv[1].upper()
   else:
      print usage
      return
   
   st = smokeTest(up)
   host = st.genImage()
   st.checkLog(host)

if __name__ == "__main__":
   main(sys.argv[1:])
