USE_GUPMAKER = False
BASELINE = { }
BUNDLED_LMC = { }

UP = {"VRCS64-UP": ('CXS2010009_1', ['mw', 'dummy', 'ee'], 'BASEBAND')}

LMC = {
      'mw':    ("VRCS64-MW", "CXP2030009_1", "VRCS64-MW" , "CXC2010576_1", "CAX2010001_1"),
      'dummy': ("VRCS64-DUMMY", "CXP2030008_1", "", "", "")
}

EXT_LMC = {
      "ee": ("VRCS64-EE", "CXP2030007_1")
}

EXT2_LMC = { }

BUNDLED_LMC = { }


APPS = {
      "cec":     ("mw", "", "CEC2_CXC1734012_2"),
      "aic":     ("mw", "CXA2010009_1", "AIC2_CXC1737204_2"),
      "alh":     ("mw", "CXA2010010_1", "ALH2_CXC1735317_2"),
      "appm":     ("mw", "CXA2010011_1", "APPM2_CXC1733863_2"),
      "cch":     ("mw", "CXA2010012_1", "CCH2_CXC1739220_2"),
      "cert":     ("mw", "CXA2010013_1", "CERT2_CXC1736590_2"),
      "clh":     ("mw", "CXA2010014_1", "CLH2_CXC1735338_2"),
      "com":     ("mw", "CXA2010017_1", "COM2_CXC1733991_2"),
      "comsa":     ("mw", "CXA2010016_1", "COMSA_CXC1733851"),
      "comte":     ("mw", "", "COMTE2_CXC1733993_2"),
      "coi":     ("mw", "CXA2010015_1", "COI_CXC1737966"),
      "ecoli":     ("mw", "CXA2010018_1", "ECOLI2_CXC1736533_2"),
      "eitc":     ("mw", "", "EITC2_CXC1736958_2"),
      "eqs":     ("mw", "CXA2010019_1", "EQS2_CXC1735206_2"),
      "ftpes":   ("mw", "",            "FTPES_CXC1739914"   ),
      "gmf":     ("mw", "CXA2010020_1", "GMF2_CXC1734417_2"),
      "hwc":     ("mw", "CXA2010021_1", "HWC"),
      "jsone":   ("mw", "",            "JSONE_CXC1740260"   ),
      "lih":     ("mw", "CXA2010022_1", "LIH2_CXC1733857_2"),
      "lma":     ("mw", "CXA2010023_1", "LMA2_CXC1736657_2"),
      "log":     ("mw", "CXA2010024_1", "LOG_CXC1733858"),
      "omc":     ("mw", "CXA2010025_1", "OMC2_CXC1737109_2"),
      "oot":     ("mw", "CXA2010026_1", "OOT2_CXC1737111_2"),
      "otp":     ("mw", "", "OTP2_CXC1733859_2"),
      "pes":     ("mw", "CXA2010027_1", "PES2_CXC1737851_2"),
      "pms":     ("mw", "CXA2010028_1", "PMS2_CXC1733860_2"),
      "saf":     ("mw", "CXA2010029_1", "SAF2_CXC1735556_2"),
      "safc":     ("mw", "", "SAF2_CXC1735556_2"),
      "safs":     ("mw", "", "SAF2_CXC1735556_2"),
      "svnfmd":  ("mw", "CXA2010098_1", "SVNFMD"            ),
      "swm":     ("mw", "CXA2010030_1", "SWM_CXC1733929"),
      "sys":     ("mw", "CXA2010031_1", "SYS4_CXC1733862_4"),
      "sysinit":     ("mw", "", "SYS4_CXC1733862_4"),
      "sysdb":     ("mw", "", "SYS4_CXC1733862_4"),
      "tim":     ("mw", "CXA2010032_1", "TIM2_CXC1738528_2"),
      "vnfc":    ("mw", "",            "VNFC"           ),

      "fake": ("dummy", "", "FAKE_CXC1734197_4"),
      "ift":  ("dummy", "", "IFT_CXC1735023_4"),
      "test/otp":  ("dummy", "", "OTP12_CXC1733859_12"),
}

SRCDIR = [
      'CEC',
      'AIC',
      'ALH',
      'APPM',
      'CCH',
      'CERT',
      'CLH',
      'COM',
      'COMSA',
      'COMTE',
      'COI',
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
      'SVNFMD',
      'SYS',
      'TIM',
      'VNFC',
      'test/OTP',
      'test/IFT',
      'test/FAKE',
      ]
