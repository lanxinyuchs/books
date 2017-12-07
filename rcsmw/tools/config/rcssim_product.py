import os
execfile(os.path.join(os.environ['TOPDIR'], 'tools', 'config', 'g2_common.py'))

BASELINE = {}
UP = {"RCP-SIM": ('CXS2010025_1', ['mw', 'dummy'], 'BASEBAND')}
BUNDLED_LMC = { }

EXT_LMC = {}

# TODO: Check if we need new CXC number
LMC = {
      'mw':    ("RCS-SIM", "CXP2010042_1", "RCSSIM-MW", "CXC2010721_1", "CAX2010001_3"),
      'dummy': ("DUMMY-SIM", "CXP2020303_1", "", "", "")
}

APPS = {
      "aic":     ("mw", "CXA2010080_1",  "AIC2_CXC1737204_2"  ),
      "alh":     ("mw", "CXA2010066_1",  "ALH2_CXC1735317_2"  ),
      "appm":    ("mw", "CXA2010081_1",  "APPM2_CXC1733863_2" ),
      "cch":     ("mw", "CXA2010069_1", "CCH2_CXC1739220_2"  ),
      "cec":     ("mw", "",            "CEC2_CXC1734012_2"  ),
      "cert":    ("mw", "CXA2010083_1",  "CERT2_CXC1736590_2" ),
      "clh":     ("mw", "CXA2010073_1",  "CLH2_CXC1735338_2"  ),
      "coi":     ("mw", "CXA2010070_1",  "COI_CXC1737966"     ),
      "comsa":   ("mw", "CXA2010074_1",  "COMSA_CXC1733851"   ),
      "com":     ("mw", "CXA2010067_1",  "COM2_CXC1733991_2"  ),
      "comte":   ("mw", "",            "COMTE2_CXC1733993_2"),
      "ecoli":   ("mw", "CXA2010075_1",  "ECOLI2_CXC1736533_2"),
      "eitc":    ("mw", "",            "EITC2_CXC1736958_2" ),
      "eqs":     ("mw", "CXA2010076_1",  "EQS2_CXC1735206_2"  ),
      "ftpes":   ("mw", "",            "FTPES_CXC1739914"   ),
      "gmf":     ("mw", "CXA2010086_1",  "GMF2_CXC1734417_2"  ),
      "hwc":     ("mw", "CXA2010068_1", "HWC"                ),
      "lih":     ("mw", "CXA2010085_1",  "LIH2_CXC1733857_2"  ),
      "lma":     ("mw", "CXA2010071_1",  "LMA2_CXC1736657_2"  ),
      "log":     ("mw", "CXA2010082_1",  "LOG_CXC1733858"     ),
      "omc":     ("mw", "CXA2010084_1",  "OMC2_CXC1737109_2"  ),
      "oot":     ("mw", "CXA2010078_1",  "OOT2_CXC1737111_2"  ),
      "otp":     ("mw", "",            "OTP2_CXC1733859_2"  ),
      "pes":     ("mw", "CXA2010089_1",  "PES2_CXC1737851_2"  ),
      "pms":     ("mw", "CXA2010072_1",  "PMS2_CXC1733860_2"  ),
      "saf":     ("mw", "CXA2010079_1",  "SAF2_CXC1735556_2"  ),
      "safs":    ("mw", "",            "SAF2_CXC1735556_2"  ),
      "safc":    ("mw", "",            "SAF2_CXC1735556_2"  ),
      "swm":     ("mw", "CXA2010088_1",  "SWM_CXC1733929"     ),
      "sys":     ("mw", "CXA2010087_1",  "SYS2_CXC1733862_2"  ),
      "sysinit": ("mw", "",            "SYS2_CXC1733862_2"  ),
      "sysdb":   ("mw", "",            "SYS2_CXC1733862_2"  ),
      "tim":     ("mw", "CXA2010077_1", "TIM2_CXC1738528_2"  ),

      "fake":    ("dummy", "", "FAKE2_CXC1734197_2"),
      "ift":     ("dummy", "", "IFT2_CXC1735023_2"),
      "test/otp": ("dummy", "", "OTP12_CXC1733859_12"),
}
