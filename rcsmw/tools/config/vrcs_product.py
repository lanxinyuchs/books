BASELINE = { }
BUNDLED_LMC = { }
USE_GUPMAKER = False

UP = { "VRCS-UP":  ('CXS101657_4', ['mw', 'dummy', 'ee'], 'BASEBAND')}

LMC = {
      'mw':    ("VRCS-MW", "CXP9029176_4", "VRCS-MW" , "CXC1739608_4", "CAX1034072"),
      'dummy': ("VRCS-DUMMY", "CXP9029207_4", "", "", "")
}

EXT_LMC = {
      "ee": ("VRCS-EE", "CXP9029177_4")
}

EXT2_LMC = { }


BUNDLED_LMC = { }


APPS = {
      "aic":     ("mw", "CXA11480_4",  "AIC2_CXC1737204_2"  ),
      "alh":     ("mw", "CXA11457_4",  "ALH2_CXC1735317_2"  ),
      "appm":    ("mw", "CXA11417_4",  "APPM2_CXC1733863_2" ),
      "cch":     ("mw", "CXA114004_4", "CCH2_CXC1739220_2"  ),
      "cec":     ("mw", "",            "CEC2_CXC1734012_2"  ),
      "cert":    ("mw", "CXA11474_4",  "CERT2_CXC1736590_2" ),
      "clh":     ("mw", "CXA11407_4",  "CLH2_CXC1735338_2"  ),
      "coi":     ("mw", "CXA11497_4",  "COI_CXC1737966"     ),
      "comsa":   ("mw", "CXA11418_4",  "COMSA_CXC1733851"   ),
      "com":     ("mw", "CXA11462_4",  "COM2_CXC1733991_2"  ),
      "comte":   ("mw", "",            "COMTE2_CXC1733993_2"),
      "ecoli":   ("mw", "CXA11473_4",  "ECOLI2_CXC1736533_2"),
      "eitc":    ("mw", "",            "EITC2_CXC1736958_2" ),
      "eqs":     ("mw", "CXA11446_4",  "EQS2_CXC1735206_2"  ),
      "ftpes":   ("mw", "",            "FTPES_CXC1739914"   ),
      "gmf":     ("mw", "CXA11422_4",  "GMF2_CXC1734417_2"  ),
      "hwc":     ("mw", "CXA114005_4", "HWC"                ),
      "jsone":   ("mw", "",            "JSONE_CXC1740260"   ),
      "lih":     ("mw", "CXA11421_4",  "LIH2_CXC1733857_2"  ),
      "lma":     ("mw", "CXA11475_4",  "LMA2_CXC1736657_2"  ),
      "log":     ("mw", "CXA11425_4",  "LOG_CXC1733858"     ),
      "omc":     ("mw", "CXA11498_4",  "OMC2_CXC1737109_2"  ),
      "oot":     ("mw", "CXA11478_4",  "OOT2_CXC1737111_2"  ),
      "otp":     ("mw", "",            "OTP2_CXC1733859_2"  ),
      "pes":     ("mw", "CXA11494_4",  "PES2_CXC1737851_2"  ),
      "pms":     ("mw", "CXA11463_4",  "PMS2_CXC1733860_2"  ),
      "saf":     ("mw", "CXA11461_4",  "SAF2_CXC1735556_2"  ),
      "safs":    ("mw", "",            "SAF2_CXC1735556_2"  ),
      "safc":    ("mw", "",            "SAF2_CXC1735556_2"  ),
      "svnfmd":  ("mw", "CXA2010092_1","SVNFMD"             ),
      "swm":     ("mw", "CXA11423_4",  "SWM_CXC1733929"     ),
      "sys":     ("mw", "CXA11464_4",  "SYS4_CXC1733862_4"  ),
      "sysinit": ("mw", "",            "SYS4_CXC1733862_4"  ),
      "sysdb":   ("mw", "",            "SYS4_CXC1733862_4"  ),
      "tim":     ("mw", "CXA114001_4", "TIM2_CXC1738528_2"  ),
      "vnfc":    ("mw", "",            "VNFC"               ),

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
      'SVNFMD',
      'SWM',
      'SYS',
      'TIM',
      'VNFC',
      'test/OTP',
      'test/IFT',
      'test/FAKE',
      ]
