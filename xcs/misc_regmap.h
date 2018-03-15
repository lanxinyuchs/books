/*
 * This file was generated from misc_regmap.xls
 * by /home/emaland/backup/work/memMapParser/regmap2h.py on 2015-05-18 12:24:58.502120
*/

#define MISC_HW_ID                (0*4)  /* Hardware Identification register */
#define MISC_HW_REV               (1*4)  /* Hardware Revision register */
#define MISC_DEVICE_ADDR          (2*4)  /* Device address register */
#define MISC_RESET_CAUSE_0        (3*4)  /* Reset cause register for CPU 0 */
#define MISC_RESET_CAUSE_1        (4*4)  /* Reset cause register for CPU 1 */
#define MISC_RESET_MEM_0          (5*4)  /* Reset scratch pad register for XP platform */
#define MISC_RESET_MEM_1          (6*4)  /* Reset scratch pad register for radio platform */
#define MISC_ECO                  (7*4)  /* ECO register */
#define MISC_RESET_CMD            (8*4)  /* Reset command register */
#define MISC_DEBUG                (9*4)  /* Debug register */
#define MISC_MASTER_ID            (10*4)  /* AXI Master Id Register */
#define MISC_CPU_SS_PAR_STATUS    (11*4)  /* L1 and L2 Cache parity/ECC error status register */
#define MISC_STAT                 (12*4)  /* Status register */
#define MISC_RESET_IMMINENT_DELAY (13*4)  /* Reset imminent delay */
#define MISC_LTU_SYSREF           (14*4)  /* SYSREF control */
#define MISC_SERDES_SOUTH         (15*4)  /* Serdes South Control Register */
#define MISC_GROUP0               (16*4)  /* Misc group0 register */
#define MISC_GROUP1               (23*4)  /* Misc group1 register */
#define MISC_GROUP2               (30*4)  /* Misc group2 register */
#define MISC_GROUP3               (37*4)  /* Misc group3 register */
#define MISC_SENSOR_CTRL          (44*4)  /* Sensor control */
#define MISC_SENSOR_START         (45*4)  /* Sensor start */
#define MISC_SENSOR_RESULT        (46*4)  /* TempVolt result */
#define MISC_RING_OSC_RESULT      (47*4)  /* Ring oscillator result */
#define MISC_UPTIME_0             (48*4)  /* Uptime counter */
#define MISC_UPTIME_1             (49*4)  /* Uptime counter */
#define MISC_SVB                  (50*4)  /* SVB bits 0-3 */
#define MISC_DDR_RESET            (51*4)  /* DDR3 Reset control */
#define MISC_AXI_UART_BYPASS      (52*4)  /* AXI UART bypass control */
#define MISC_MONITORS_IRQ         (128*4)  /* Monitors & sensors interrupt register */
#define MISC_SRAM_PAR_ERR_IRQ     (134*4)  /* SRAM parity error interrupt register */
#define MISC_SERDES_CTRL_IRQ      (140*4)  /* Serdes Controllers interrupt register */
#define MISC_ADC_DAC_IRQ          (146*4)  /* ADC and DAC interrupt register */
#define MISC_MMI_IRQ              (152*4)  /* MMI interrupt register */
#define MISC_DL_IRQ               (158*4)  /* DL interrupt register */
#define MISC_UL_IRQ               (164*4)  /* UL interrupt register */
#define MISC_PLL_FRAC_CFG_0       (256*4)  /* PLL Frac Configuration Register #0 */
#define MISC_PLL_FRAC_CFG_1       (257*4)  /* PLL Frac Configuration Register #1 */
#define MISC_PLL_FRAC_CFG_2       (258*4)  /* PLL Frac Configuration Register #2 */
#define MISC_PLL_FRAC_CFG_3       (259*4)  /* PLL Frac Configuration Register #3 */
#define MISC_PLL_FRAC_CFG_4       (260*4)  /* PLL Frac Configuration Register #4 */
#define MISC_PLL_FRAC_CFG_5       (261*4)  /* PLL Frac Reset Control Register */
#define MISC_PLL_IF_CFG_0         (264*4)  /* PLL IF Configuration Register #0 */
#define MISC_PLL_IF_CFG_1         (265*4)  /* PLL IF Configuration Register #1 */
#define MISC_PLL_IF_CFG_2         (266*4)  /* PLL IF Reset Control Register */
#define MISC_PLL_OBS_CTRL         (267*4)  /* PLL OBS Control Register */
#define MISC_PLL_OBS_STATUS       (268*4)  /* PLL OBS Status Register */
#define MISC_DP_CTRL_0            (384*4)  /* DP0 control register */
#define MISC_DP_STATUS_0          (385*4)  /* DP0 status register */
#define MISC_DP_CTRL_1            (386*4)  /* DP1 control register */
#define MISC_DP_STATUS_1          (387*4)  /* DP1 status register */
#define MISC_DP_CTRL_2            (388*4)  /* DP2 control register */
#define MISC_DP_STATUS_2          (389*4)  /* DP2 status register */
#define MISC_DP_CTRL_3            (390*4)  /* DP3 control register */
#define MISC_DP_STATUS_3          (391*4)  /* DP3 status register */
#define MISC_DP_CTRL_4            (392*4)  /* DP4 control register */
#define MISC_DP_STATUS_4          (393*4)  /* DP4 status register */
#define MISC_DP_CTRL_5            (394*4)  /* DP5 control register */
#define MISC_DP_STATUS_5          (395*4)  /* DP5 status register */
#define MISC_DP_CTRL_6            (396*4)  /* DP6 control register */
#define MISC_DP_STATUS_6          (397*4)  /* DP6 status register */
#define MISC_DP_CTRL_7            (398*4)  /* DP7 control register */
#define MISC_DP_STATUS_7          (399*4)  /* DP7 status register */
#define MISC_DP_NMI               (400*4)  /* DP non-maskable interrupt control register */
#define MISC_PAD_LOCK             (512*4)  /* Lock register for pad control */
#define MISC_PAD_MC_0             (513*4)  /* PAD MC signal 0-2 */
#define MISC_TPIU                 (514*4)  /* Controls pads of TPIU */
#define MISC_DBG_LOCK             (640*4)  /* Lock register for debug commands */
#define MISC_DBG_RESET            (641*4)  /* Debug control for CPU Subsys and DP Subsys */
#define MISC_DBG_CTRL             (642*4)  /* Debug control for CPU Subsys and DP Subsys */
#define MISC_CPU_SS_LOCK          (768*4)  /* Lock register for CPU commands */
#define MISC_CPU_SS_CTRL          (769*4)  /* CPU ctrl register */
#define MISC_SYSRAM_CTRL          (770*4)  /* SYSRAM ctrl register */
#define MISC_BOOT_MODE            (771*4)  /* Boot source selector */
