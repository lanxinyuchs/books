import os
execfile(os.path.join(os.environ['TOPDIR'], 'tools', 'config', 'g2_common.py'))

USE_GUPMAKER = True
BASELINE = { "MSRCS-ALL" : "CXS2010021_1" }
BUNDLED_LMC = { }

UP = {
     "RCP-DUS2" :  ('CXS2010013_2', ['rcs_dus2', 'rcs_dus3', 'mw', 'dummy', 'cobra', 'tiger', 'mtiger'], 'BASEBAND'),
     "RCS-T" : ('CXS2010014_1', ['rcs_t', 'mw', 'dummy', 'katla'], 'BASEBAND-T')
}

EXT_LMC = {
      "rcs_dus2":  ("RCS-DUS2", "CXP2030010_1"),
      "rcs_t":     ("RCS-T", "CXP2030011_1"),
      "rcs_dus3":  ("RCSEE-DUS3", "CXP2020231_1"),
      "cobra":     ("COBRA", "CXP102171_1"),
      "katla":     ("KATLA", "CXP102185_1"),
      "tiger":     ("TIGER", "CXP102194_1"),
      "mtiger":    ("MTIGER", "CXP102201_1")
}

LMC = {
      'mw':    ("RCSMW-ARM", "CXP2020233_1", "MSRCS-MW", "CXC2010721_1", "CAX2010001_3"),
      'dummy': ("DUMMY-ARM", "CXP2020234_1", "", "", ""),
}

APPS = {
      "aic":     ("mw", "CXA2010080_1",  "AIC3_CXC1737204_3"  ),
      "alh":     ("mw", "CXA2010066_1",  "ALH3_CXC1735317_3"  ),
      "appm":    ("mw", "CXA2010081_1",  "APPM3_CXC1733863_3" ),
      "cch":     ("mw", "CXA2010069_1", "CCH3_CXC1739220_3"  ),
      "cec":     ("mw", "",            "CEC3_CXC1734012_3"  ),
      "cert":    ("mw", "CXA2010083_1",  "CERT3_CXC1736590_3" ),
      "clh":     ("mw", "CXA2010073_1",  "CLH3_CXC1735338_3"  ),
      "coi":     ("mw", "CXA2010070_1",  "COI_CXC1737966"     ),
      "comsa":   ("mw", "CXA2010074_1",  "COMSA_CXC1733851"   ),
      "com":     ("mw", "CXA2010067_1",  "COM3_CXC1733991_3"  ),
      "comte":   ("mw", "",            "COMTE3_CXC1733993_3"),
      "ecoli":   ("mw", "CXA2010075_1",  "ECOLI3_CXC1736533_3"),
      "eitc":    ("mw", "",            "EITC3_CXC1736958_3" ),
      "eqs":     ("mw", "CXA2010076_1",  "EQS3_CXC1735206_3"  ),
      "ftpes":   ("mw", "",            "FTPES_CXC1739914"   ),
      "gmf":     ("mw", "CXA2010086_1",  "GMF3_CXC1734417_3"  ),
      "hwc":     ("mw", "CXA2010068_1", "HWC"                ),
      "lih":     ("mw", "CXA2010085_1",  "LIH3_CXC1733857_3"  ),
      "lma":     ("mw", "CXA2010071_1",  "LMA3_CXC1736657_3"  ),
      "log":     ("mw", "CXA2010082_1",  "LOG_CXC1733858"     ),
      "omc":     ("mw", "CXA2010084_1",  "OMC3_CXC1737109_3"  ),
      "oot":     ("mw", "CXA2010078_1",  "OOT3_CXC1737111_3"  ),
      "otp":     ("mw", "",            "OTP3_CXC1733859_3"  ),
      "pes":     ("mw", "CXA2010089_1",  "PES3_CXC1737851_3"  ),
      "pms":     ("mw", "CXA2010072_1",  "PMS3_CXC1733860_3"  ),
      "saf":     ("mw", "CXA2010079_1",  "SAF3_CXC1735556_3"  ),
      "safs":    ("mw", "",            "SAF3_CXC1735556_3"  ),
      "safc":    ("mw", "",            "SAF3_CXC1735556_3"  ),
      "swm":     ("mw", "CXA2010088_1",  "SWM_CXC1733929"     ),
      "sys":     ("mw", "CXA2010087_1",  "SYS3_CXC1733862_3"  ),
      "sysinit": ("mw", "",            "SYS3_CXC1733862_3"  ),
      "sysdb":   ("mw", "",            "SYS3_CXC1733862_3"  ),
      "tim":     ("mw", "CXA2010077_1", "TIM3_CXC1738528_3"  ),

      "fake":    ("dummy", "", "FAKE3_CXC1734197_3"),
      "ift":     ("dummy", "", "IFT3_CXC1735023_3"),
      "test/otp": ("dummy", "", "OTP13_CXC1733859_13"),
}
