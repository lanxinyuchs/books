#!/usr/bin/env python
# encoding: utf-8
# Przemyslaw Rzepecki, 2016

"""
Erlang support
"""

from waflib import Task, TaskGen
from waflib.TaskGen import extension, feature, after_method, before_method
import os
import re
import threading

def scan_meth(task): 
   node = task.inputs[0] 
   parent = node.parent

   deps = []
   scanned = set([])
   nodes_to_scan = [node]

   for n in nodes_to_scan:
       if n.abspath() in scanned:
           continue

       for i in re.findall('-include\("(.*)"\)\.', n.read()):
          found = False
          for d in task.includes_nodes:
             r = task.generator.path.find_resource(os.path.join(d,i))
             if r:
                 deps.append(r)
                 nodes_to_scan.append(r)
                 found = True
                 break
             r = task.generator.bld.root.find_resource(os.path.join(d,i))
             if r:
                 deps.append(r)
                 nodes_to_scan.append(r)
                 found = True
                 break
          if not found:
             pass
       scanned.add(n.abspath())

   return (deps, [])

def configure(conf):
        conf.find_program('erlc', var='ERLC')
        conf.find_program('mpDtdParser.escript', var='MP_DTD_PARSER')
        conf.find_program('objcopy', var='OBJCOPY')
        conf.find_program('strip', var='STRIP')
        conf.add_os_flags('ERLC_FLAGS')
        conf.add_os_flags('ERL_FLAGS')
        conf.add_os_flags('MP_DTD_PARSER_FLAGS')


@TaskGen.extension('.erl')
def process(self, node):
        tsk = self.create_task('erl', node, node.change_ext('.beam'))
        tsk.includes_nodes = self.to_list(getattr(self, 'includes', [])) + self.env['INCLUDES'] + [node.parent.abspath()]
        tsk.defines        = self.to_list(getattr(self, 'defines', [])) + self.env['DEFINES']
        tsk.flags          = self.to_list(getattr(self, 'flags', [])) + self.env['ERLC_FLAGS']
        if getattr(self, 'app', False):
           self.bld.install_files('${PREFIX}/'+getattr(self, 'app', False)+'/ebin', node.change_ext('.beam'))

@TaskGen.extension('.asn')
def process(self, node):
        tsk = self.create_task('erl', node, node.change_ext('.beam'))
        tsk.includes_nodes = self.to_list(getattr(self, 'includes', [])) + self.env['INCLUDES'] + [node.parent.abspath()]
        tsk.defines        = self.to_list(getattr(self, 'defines', [])) + self.env['DEFINES']
        tsk.flags          = self.to_list(getattr(self, 'flags', [])) + self.env['ERLC_FLAGS'] + ["-b", "der", "+debug_info"]
        self.bld.install_files('${PREFIX}/'+getattr(self, 'app', "")+'/ebin', node.change_ext('.beam'))

@TaskGen.extension('.xrl')
def process(self, node):
        tsk = self.create_task('erl', node, node.change_ext('.erl'))
        tsk.includes_nodes = []
        tsk.defines        = []
        tsk.flags          = []
        self.source.append(node.change_ext('.erl'))

@TaskGen.extension('.yrl')
def process(self, node):
        tsk = self.create_task('erl', node, node.change_ext('.erl'))
        tsk.includes_nodes = []
        tsk.defines        = []
        tsk.flags          = []
        self.source.append(node.change_ext('.erl'))

class erl(Task.Task): 
   scan=scan_meth 
   color='GREEN'
   vars = ['ERLC_FLAGS', 'ERLC', 'ERL', 'INCLUDES', 'DEFINES']
   def run(self):
      output=self.inputs[0].change_ext('.beam')
      erlc = self.generator.env["ERLC"]
      inca = [i for i in self.includes_nodes if os.path.isabs(i)]
      incr = [self.generator.path.find_dir(i) for i in self.includes_nodes if not os.path.isabs(i)]
      incr = filter(lambda x:x, incr)
      incb = [i.get_bld() for i in incr]
      inc = inca + [i.abspath() for i in incr+incb]
      defines = " ".join(["-D"+d for d in self.defines])
      r = self.exec_command(
               erlc + self.flags + ['+debug_info', '+{attribute,insert,vsn,"%s"}'%self.generator.env["ERLVSN"]]
               + ["-I"+i for i in inc]
               + ["-D"+d for d in self.defines]
               + [self.inputs[0].path_from(output.parent)],
               cwd=output.parent.abspath(),
               shell=False)
      return r


@TaskGen.extension('.beam')
def process(self, node):
    pass


class erl_test(Task.Task):
        color = 'BLUE'
        vars = ['ERL_EXEC_FLAGS', 'ERL_RUNNER']

        def run(self):
            test_list = ", ".join([m.change_ext("").path_from(m.parent)+":test()" for m in self.modules])
            environ   = " ".join(self.environ)
            flags = " ".join(self.flags)
            print test_list, environ, flags
            return self.exec_command("""%s erl %s -sasl errlog_type error -noinput -eval '
                    halt(
                       case lists:all(fun(Elem) -> Elem == ok end, [%s]) of
                          true  -> 0;
                          false -> 1
                       end).' """% (environ, flags, test_list), cwd = self.modules[0].parent.abspath())

@feature('eunit')
@after_method('process_source')
def addtestrun(self):
        test_modules = [t.outputs[0] for t in self.tasks]
        test_task = self.create_task('erl_test')
        test_task.set_inputs(self.source + test_modules)
        test_task.modules = test_modules
        test_task.flags = self.to_list(getattr(self, 'flags', []))
        test_task.environ = self.to_list(getattr(self, 'environ', []))

TaskGen.declare_chain(name = 'mpDtdParser',
        rule      = '${MP_DTD_PARSER} ${MP_DTD_PARSER_FLAGS} -out ${TGT[0].parent.abspath()} ${SRC[0].abspath()}',
        ext_in    = '.xml',
        ext_out   = '.hrl',
        shell     = False)

class proto2e(Task.Task): 
   color='BLUE'
   vars = ['ERL_LIBS', 'ERL']
   def run(self):

      # TODO: remove /proj/... use some configure flag
      return self.exec_command(
            '%(ERL)s -boot start_clean -noshell %(ERL_FLAGS)s \
         -I %(SDIR)s\
         -o %(TDIR)s\
         -s gpb_compile c %(NAME)s\
         -g "Src: %(NAME)s@@" -extra -strbin' % {
            "ERL": " ".join(self.generator.env["ERL"]),
            "ERL_FLAGS": " ".join(self.generator.env["ERL_FLAGS"]),
            "SDIR": self.inputs[0].parent.abspath(),
            "SRC": self.inputs[0].abspath(),
            "TDIR": self.outputs[0].parent.abspath(),
            "NAME": self.inputs[0].name,
            })

class proto2c(Task.Task): 
   color='BLUE'
   vars = ['PROTOC_C']
   def run(self):
      return self.exec_command(
            'cd %(SDIR)s && %(PROTOC_C)s\
            -I %(SDIR)s\
            --c_out %(TDIR)s %(SRC)s' % {
               "SRC": self.inputs[0].abspath(),
               "SDIR": self.inputs[0].parent.abspath(),
               "TDIR": self.outputs[0].parent.abspath(),
               "PROTOC_C": " ".join(self.generator.env["PROTOC_C"])
               })

@TaskGen.extension('.proto')
def method(self, node):
   tsk1 = self.create_task('proto2c', node, node.change_ext('.pb-c.c'))
   tsk2 = self.create_task('proto2e', node, node.change_ext('.erl'))
   tsk3 = self.create_task('erl', node.change_ext('.erl'), node.change_ext('.beam'))
   tsk3.includes_nodes = self.to_list(getattr(self, 'includes', [])) + self.env['INCLUDES'] + [node.parent.abspath()]
   tsk3.defines        = self.to_list(getattr(self, 'defines', [])) + self.env['DEFINES']
   tsk3.after.append(tsk2)
   tsk3.flags          = self.to_list(getattr(self, 'flags', [])) + self.env['ERLC_FLAGS']
   self.bld.install_files('${PREFIX}/'+getattr(self, 'app', "")+'/ebin', node.change_ext('.beam'))
   self.source.append(node.change_ext('.pb-c.c'))


class AppSrc(Task.Task): 
   color='YELLOW'
   vars = ['VSN']
   def run(self):
      modules = ",\n\t".join(["'%s'"% os.path.basename(str(m.change_ext(""))) for m in self.modules])
      modules = "[%s]" % modules
      cont = self.inputs[0].read()
      cont = cont.replace("&id&", self.id, 1)
      cont = cont.replace("&vsn&", self.vsn, 1)
      cont = cont.replace('"&modules&"', modules, 1)
      self.outputs[0].write(cont)

@TaskGen.extension('.appSrc')
def process(self, node):
   tsk = self.create_task('AppSrc', node, node.change_ext('.app'))
   tsk.modules = self.to_list(getattr(self, 'modules', []))
   tsk.id      = getattr(self, 'id', "ID??")
   tsk.vsn     = getattr(self, 'vsn', "VSN??")
   self.bld.install_files('${PREFIX}/'+getattr(self, 'app', "")+'/ebin', node.change_ext('.app'))


@after_method('apply_link')
@feature('cprogram')
@feature('cshlib')
def add_strip_task(self):
   if not hasattr(self, 'install_path') or not self.install_path:
      return
   install_path = self.install_path
   target = self.link_task.outputs[0]
   tdbg   = target.change_ext(".debug_info")
   self.link_task.outputs.append(tdbg)
   self.bld.install_files("debug/"+install_path, tdbg) 

def wrap_compiled_task(classname):
    # override the class to add a new 'run' method
    # such an implementation guarantees that the absence of race conditions
    #  
    cls1 = Task.classes[classname]
    cls2 = type(classname, (cls1,), {'run_str': "${OBJCOPY} --only-keep-debug ${TGT[0].abspath()} ${TGT[0].change_ext('.debug_info').abspath()}"})
    cls3 = type(classname, (cls2,), {'run_str': '${STRIP} ${TGT[0].abspath()}'})
    cls4 = type(classname, (cls3,), {})
    
    def run_all(self):
        ret = cls1.run(self)
        if ret:
        	return ret
        ret = cls2.run(self)
        if ret:
        	return ret
        ret = cls3.run(self)
        return ret
    cls4.run = run_all

class edoc(Task.Task):
   vars = ['ERL_LIBS', 'ERL']
   def run(self):
      self.exec_command(self.generator.env.ERL
            + self.generator.env.ERL_FLAGS
            + ['-boot', 'start_clean', '-noshell', '-noinput',
               '-eval', 'edoc:files([\"'+self.inputs[0].abspath()+'\"]), halt(0).'],
            cwd = self.outputs[0].parent.abspath()
            )

@feature('edoc')
@before_method('process_source')
def add_edoc_task(self):
   self.meths.remove('process_source') # don't process source, it would creat double erl->beam task
   e = self.path.find_resource(self.source)
   t = e.change_ext('.html')
   png = t.parent.make_node('erlang.png')
   css = t.parent.make_node('stylesheet.css')
   self.create_task('edoc', e, [t, png, css])
   self.bld.install_files(self.install_path, [t, png, css])

for k in 'cprogram cshlib cxxprogram cxxshlib'.split():
    if k in Task.classes:
	wrap_compiled_task(k)
