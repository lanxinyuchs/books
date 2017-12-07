USE_GUPMAKER = True
BASELINE = { }

UP = {"BRCS-UP": ('CXS101665_5', ['rcs', 'dummy', 'cobra'], 'BASEBAND')}

BUNDLED_LMC = {
      'rcs': ('BRCS', 'CXP9032853_5', ['mw', 'ee'])
}

EXT_LMC = {
      "ee":        ("BRCS-EE", "CXP9029438_5"),
      "cobra":     ("COBRA", "CXP102171_1")
}

EXT2_LMC = {}

LMC = {
      'mw':    ("BRCS-MW", "CXP9029439_5", "BRCS-MW", "CXC1739608_5", "CAX2010001_2"),
      'dummy': ("BRCS-DUMMY", "CXP9029440_5", "", "", "")
}

APPS = {
      "aic":     ("mw", "CXA11480_5",  "AIC3_CXC1737204_3"  ),
      "alh":     ("mw", "CXA11457_5",  "ALH3_CXC1735317_3"  ),
      "appm":    ("mw", "CXA11417_5",  "APPM3_CXC1733863_3" ),
      "cch":     ("mw", "CXA114004_5", "CCH3_CXC1739220_3"  ),
      "cec":     ("mw", "",            "CEC3_CXC1734012_3"  ),
      "cert":    ("mw", "CXA11474_5",  "CERT3_CXC1736590_3" ),
      "clh":     ("mw", "CXA11407_5",  "CLH3_CXC1735338_3"  ),
      "coi":     ("mw", "CXA11497_5",  "COI_CXC1737966"     ),
      "comsa":   ("mw", "CXA11418_5",  "COMSA_CXC1733851"   ),
      "com":     ("mw", "CXA11462_5",  "COM3_CXC1733991_3"  ),
      "comte":   ("mw", "",            "COMTE3_CXC1733993_3"),
      "ecoli":   ("mw", "CXA11473_5",  "ECOLI3_CXC1736533_3"),
      "eitc":    ("mw", "",            "EITC3_CXC1736958_3" ),
      "eqs":     ("mw", "CXA11446_5",  "EQS3_CXC1735206_3"  ),
      "ftpes":   ("mw", "",            "FTPES_CXC1739914"   ),
      "gmf":     ("mw", "CXA11422_5",  "GMF3_CXC1734417_3"  ),
      "hwc":     ("mw", "CXA114005_5", "HWC"                ),
      "jsone":   ("mw", "",            "JSONE_CXC1740260"   ),
      "lih":     ("mw", "CXA11421_5",  "LIH3_CXC1733857_3"  ),
      "lma":     ("mw", "CXA11475_5",  "LMA3_CXC1736657_3"  ),
      "log":     ("mw", "CXA11425_5",  "LOG_CXC1733858"     ),
      "omc":     ("mw", "CXA11498_5",  "OMC3_CXC1737109_3"  ),
      "oot":     ("mw", "CXA11478_5",  "OOT3_CXC1737111_3"  ),
      "otp":     ("mw", "",            "OTP3_CXC1733859_3"  ),
      "pes":     ("mw", "CXA11494_5",  "PES3_CXC1737851_3"  ),
      "pms":     ("mw", "CXA11463_5",  "PMS3_CXC1733860_3"  ),
      "saf":     ("mw", "CXA11461_5",  "SAF3_CXC1735556_3"  ),
      "safs":    ("mw", "",            "SAF3_CXC1735556_3"  ),
      "safc":    ("mw", "",            "SAF3_CXC1735556_3"  ),
      "swm":     ("mw", "CXA11423_5",  "SWM_CXC1733929"     ),
      "sys":     ("mw", "CXA11464_5",  "SYS3_CXC1733862_3"  ),
      "sysinit": ("mw", "",            "SYS3_CXC1733862_3"  ),
      "sysdb":   ("mw", "",            "SYS3_CXC1733862_3"  ),
      "tim":     ("mw", "CXA114001_5", "TIM3_CXC1738528_3"  ),
      "vnfc":    ("mw", "",            "VNFC"           ),

      "fake": ("dummy", "", "FAKE_CXC1734197_3"),
      "ift":  ("dummy", "", "IFT_CXC1735023_3"),
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
      'SYS',
      'TIM',
      'VNFC',
      'test/OTP',
      'test/IFT',
      'test/FAKE',
      ]
