USE_GUPMAKER = True
BASELINE = { }
BUNDLED_LMC = { }
EXT_LMC = { }

UP =	{
	"RCS-SIM":  ("CXS2010025_1", ["sim32-g2ee", "dummy-sim", "rcsmw-sim", ], "BASEBAND"),
}
USE_GUPMAKER = True

EXT2_LMC =	{
	"sim32-g2ee":	("SIM32-G2EE", "CXP2030019_1", "R1A151", "file:///proj/5G_rcs/cache/SIM32-G2EE/R1A151/SIM32-G2EE_CXP2030019_1-R1A151.cxp"),
}

LMC =	{
	"dummy-sim":	("DUMMY-SIM", "CXP2020303_1", "RCS-DUMMY", "CXC2010928_1", "CAX2010001_1"),
	"rcsmw-sim":	("RCSMW-SIM", "CXP2010042_1", "RCSMW", "CXC2010721_1", "CAX2010001_1"),
}

APPS = {
	"cec":	("rcsmw-sim", "", "RCSMW_CXC2010721_1" ),
	"aic":	("rcsmw-sim", "CXA2010080_1", "RCSMW_CXC2010721_1" ),
	"alh":	("rcsmw-sim", "CXA2010066_1", "RCSMW_CXC2010721_1" ),
	"appm":	("rcsmw-sim", "CXA2010081_1", "RCSMW_CXC2010721_1" ),
	"cch":	("rcsmw-sim", "CXA2010069_1", "RCSMW_CXC2010721_1" ),
	"cert":	("rcsmw-sim", "CXA2010083_1", "RCSMW_CXC2010721_1" ),
	"clh":	("rcsmw-sim", "CXA2010073_1", "RCSMW_CXC2010721_1" ),
	"coi":	("rcsmw-sim", "CXA2010070_1", "RCSMW_CXC2010721_1" ),
	"com":	("rcsmw-sim", "CXA2010067_1", "RCSMW_CXC2010721_1" ),
	"comsa":	("rcsmw-sim", "CXA2010074_1", "RCSMW_CXC2010721_1" ),
	"comte":	("rcsmw-sim", "", "RCSMW_CXC2010721_1" ),
	"ecoli":	("rcsmw-sim", "CXA2010075_1", "RCSMW_CXC2010721_1" ),
	"eitc":	("rcsmw-sim", "", "RCSMW_CXC2010721_1" ),
	"eqs":	("rcsmw-sim", "CXA2010076_1", "RCSMW_CXC2010721_1" ),
	"ftpes":	("rcsmw-sim", "", "RCSMW_CXC2010721_1" ),
	"gmf":	("rcsmw-sim", "CXA2010086_1", "RCSMW_CXC2010721_1" ),
	"hwc":	("rcsmw-sim", "CXA2010068_1", "RCSMW_CXC2010721_1" ),
	"jsone":	("rcsmw-sim", "", "RCSMW_CXC2010721_1" ),
	"lih":	("rcsmw-sim", "CXA2010085_1", "RCSMW_CXC2010721_1" ),
	"lma":	("rcsmw-sim", "CXA2010071_1", "RCSMW_CXC2010721_1" ),
	"log":	("rcsmw-sim", "CXA2010082_1", "RCSMW_CXC2010721_1" ),
	"omc":	("rcsmw-sim", "CXA2010084_1", "RCSMW_CXC2010721_1" ),
	"oot":	("rcsmw-sim", "CXA2010078_1", "RCSMW_CXC2010721_1" ),
	"otp":	("rcsmw-sim", "", "RCSMW_CXC2010721_1" ),
	"pes":	("rcsmw-sim", "CXA2010089_1", "RCSMW_CXC2010721_1" ),
	"pms":	("rcsmw-sim", "CXA2010072_1", "RCSMW_CXC2010721_1" ),
	"saf":	("rcsmw-sim", "CXA2010079_1", "RCSMW_CXC2010721_1" ),
	"safs":	("rcsmw-sim", "", "RCSMW_CXC2010721_1" ),
	"safc":	("rcsmw-sim", "", "RCSMW_CXC2010721_1" ),
	"swm":	("rcsmw-sim", "CXA2010088_1", "RCSMW_CXC2010721_1" ),
	"sysinit":	("rcsmw-sim", "", "RCSMW_CXC2010721_1" ),
	"sysdb":	("rcsmw-sim", "", "RCSMW_CXC2010721_1" ),
	"sys":	("rcsmw-sim", "CXA2010087_1", "RCSMW_CXC2010721_1" ),
	"tim":	("rcsmw-sim", "CXA2010077_1", "RCSMW_CXC2010721_1" ),
	"fake":	("dummy-sim", "", "RCS-DUMMY_CXC2010928_1" ),
	"ift":	("dummy-sim", "", "RCS-DUMMY_CXC2010928_1" ),
	"test/otp":	("dummy-sim", "", "RCS-DUMMY_CXC2010928_1" ),
}

SRCDIR = [
	'CEC',
	'AIC',
	'ALH',
	'APPM',
	'CCH',
	'CERT',
	'CLH',
	'COI',
	'COM',
	'COMSA',
	'COMTE',
	'ECOLI',
	'EITC',
	'EQS',
	'FTPES',
	'GMF',
	'HWC',
	'JSONE',
	'LIH',
	'LMA',
	'LOG',
	'OMC',
	'OOT',
	'OTP',
	'PES',
	'PMS',
	'SAF',
	'SWM',
	'SYS',
	'TIM',
	'test/FAKE',
	'test/IFT',
	'test/OTP',
 ]
