!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE WRPLOT
!
!-------------------------Disclaimer-----------------------------------!
!
!     This material was prepared as an account of work sponsored by
!     an agency of the United States Government. Neither the
!     United States Government nor the United States Department of
!     Energy, nor Battelle, nor any of their employees, makes any
!     warranty, express or implied, or assumes any legal liability or
!     responsibility for the accuracy, completeness, or usefulness
!     of any information, apparatus, product, software or process
!     disclosed, or represents that its use would not infringe
!     privately owned rights.
!
!----------------------Acknowledgement---------------------------------!
!
!     This software and its documentation were produced with Government
!     support under Contract Number DE-AC06-76RLO-1830 awarded by the
!     United Department of Energy. The Government retains a paid-up
!     non-exclusive, irrevocable worldwide license to reproduce,
!     prepare derivative works, perform publicly and display publicly
!     by or for the Government, including the right to distribute to
!     other Government contractors.
!
!---------------------Copyright Notices--------------------------------!
!
!            Copyright Battelle Memorial Institute, 1996
!                    All Rights Reserved.
!
!----------------------Description-------------------------------------!
!
!     Write plot files
!
!   1 aqueous pressure (Pa), ' PL '
!   2 gas pressure (Pa), ' PG '
!   3 NAPL pressure (Pa), ' PN '
!   4 temperature (C), ' T  '
!   5 phase condition (null) 'PHCN'
!   6 aqueous gauge pressure (Pa), 'GPL '
!   7 gas gauge pressure (Pa), 'GPG '
!   8 NAPL gauge pressure (Pa), 'GPN '
!   9 apparent aqueous saturation (null), 'ASL '
!   10 apparent total-liquid saturation (null), 'AST '
!   11 aqueous saturation (null), ' SL '
!   12 gas saturation (null), ' SG '
!   13 NAPL saturation (null), ' SN '
!   14 total-liquid saturation (null), ' ST '
!   15 aqueous moisture content (null), 'MCL '
!   16 NAPL moisture content (null), 'MCN '
!   17 total-liquid moisture content (null), 'MCT '
!   18 effective trapped NAPL (null), 'ESNT'
!   19 effective trapped gas (null), 'ESGT'
!   20 diffusive porosity (null), 'PORD'
!   21 gas water mass fraction (null), 'XGW '
!       or volumetric component mass fraction (null), 'XVC'
!   22 gas air mass fraction (null), 'XGA '
!       or gas component mass fraction (null), 'XGC'
!   23 gas oil mass fraction (null), 'XGO '
!       or NAPL component mass fraction (null), 'XNC'
!   24 aqueous water mass fraction (null), 'XLW '
!   25 aqueous air mass fraction (null), 'XLA '
!   26 aqueous oil mass fraction (null), 'XLO '
!   27 aqueous hydraulic head (m), 'HHL '
!   28 gas hydraulic head (m), 'HHG '
!   29 NAPL hydraulic head (m), 'HHN '
!   30 rock/soil type (null), 'RSZN'
!   31 aqueous relative permeability (null), 'RKL '
!   32 gas relative permeability (null), 'RKG '
!   33 NAPL relative permeability (null), 'RKN '
!       or liquid-CO2 relative permeability (null), 'RKN '
!   34 aqueous density (kg/m^3), 'RHOL'
!   35 gas density (kg/m^3), 'RHOG'
!   36 NAPL density (kg/m^3), 'RHON'
!   37 total water mass (kg), 'TMW '
!   38 total air mass (kg), 'TMA '
!       or total component mass (kg), 'TMC'
!   39 total oil mass (kg), 'TMO '
!   40 water mass source integral (kg), 'SRIW'
!   41 air mass source integral (kg), 'SRIA'
!       or component mass source integral (kg), 'SRIC'
!   42 oil mass source integral (kg), 'SRIO'
!   43 energy source integral (W), 'SRIT'
!   44 x thermal conductivity (W/m K), 'THKX'
!   45 y thermal conductivity (W/m K), 'THKY'
!   46 z thermal conductivity (W/m K), 'THKZ'
!   47 salt volumetric concentration (kg/m^3), ' CS '
!   48 salt aqueous concentration (kg/m^3), 'CSL '
!   49 aqueous courant (null), 'CRNL'
!   50 total salt mass (kg), 'TMS '
!   51 x aqueous volumetric flux (m/s), ' UL '
!   52 y aqueous volumetric flux (m/s), ' VL '
!   53 z aqueous volumetric flux (m/s), ' WL '
!   54 x gas volumetric flux (m/s), ' UG '
!   55 y gas volumetric flux (m/s), ' VG '
!   56 z gas volumetric flux (m/s), ' WG '
!   57 x NAPL volumetric flux (m/s), ' UN '
!   58 y NAPL volumetric flux (m/s), ' VN '
!   59 z NAPL volumetric flux (m/s), ' WN '
!   60 x heat flux (W/m^2), ' UQ '
!   61 y heat flux (W/m^2), ' VQ '
!   62 z heat flux (W/m^2), ' WQ '
!   63 matric potential head (m), 'MPH '
!       or Webb matching point head (m), 'WMPH'
!   64 x salt flux (kg/m^2 s), ' US '
!   65 y salt flux (kg/m^2 s), ' VS '
!   66 z salt flux (kg/m^2 s), ' WS '
!   67 xnc salt flux (kg/m^2 s), 'USNC'
!   68 ync salt flux (kg/m^2 s), 'VSNC'
!   69 znc salt flux (kg/m^2 s), 'WSNC'
!   70 gas water mole fraction (null), 'XMGW'
!   71 gas air mole fraction (null), 'XMGA'
!       or gas component mole fraction (null), 'XMGC'
!   72 gas oil mole fraction (null), 'XMGO'
!       or NAPL component mole fraction (null), 'XMNC'
!   73 gas water concentration (kg/m^3), 'CGW '
!       or volumetric component concentration (kg/m^3), 'CVC'
!   74 gas air concentration (kg/m^3), 'CGA '
!       or gas component concentration (kg/m^3), 'CGC'
!   75 gas oil concentration (kg/m^3), 'CGO '
!       or NAPL component concentration (kg/m^3), 'CNC'
!   76 aqueous water concentration (kg/m^3), 'CLW '
!   77 aqueous air concentration (kg/m^3), 'CLA '
!   78 aqueous oil concentration (kg/m^3), 'CLO '
!   79 gas courant (null), 'CRNG'
!   80 ice pressure (Pa), 'PI '
!   80  system pressure (Pa), 'P'
!   80 stress-xx (Pa), 'SIGXX'
!   81 ice saturation (null), 'SI '
!   82 ice density (kg/m^3), 'RHOF'
!   83 aqueous matrix (null), 'DSLM'
!       or NAPL matrix (null), 'DSNM'
!       or Eclipse gas saturation (null), 'ESG'
!   84 aqueous fracture (null), 'DSLF'
!       or NAPL fracture (null), 'DSNF'
!       or Eclipse oil saturation (null), 'ESO'
!   85 gas matrix (null), 'DSGM'
!   86 gas fracture (null), 'DSGF'
!   87 xnc aqueous volumetric flux (m/s), 'ULNC'
!   88 ync aqueous volumetric flux (m/s), 'VLNC'
!   89 znc aqueous volumetric flux (m/s), 'WLNC'
!   90 xnc gas volumetric flux (m/s), 'UGNC'
!   91 ync gas volumetric flux (m/s), 'VGNC'
!   92 znc gas volumetric flux (m/s), 'WGNC'
!   93 xnc NAPL volumetric flux (m/s), 'UNNC'
!   94 ync NAPL volumetric flux (m/s), 'VNNC'
!   95 znc NAPL volumetric flux (m/s), 'WNNC'
!   96 xnc heat flux (W/m^2), 'UQNC'
!   97 ync heat flux (W/m^2), 'VQNC'
!   98 znc heat flux (W/m^2), 'WQNC'
!   99 NAPL courant (null), 'CRNN'
!   100 node number (null), 'NODE'
!   101 osmotic pressure (Pa), 'POSM'
!   101 stress-yy (Pa), 'SIGYY'
!   102 osmotic efficiency factor (null), 'OEC '
!   103 aqueous alcohol concentration (kg/m^3), 'CLA '
!   104 NAPL alcohol concentration (kg/m^3), 'CNA '
!   105 trapped gas saturation (null), 'SGT '
!   106 trapped NAPL saturation (null), 'SNT '
!   107 aqueous trapped gas (null), 'SGTL'
!   108 NAPL trapped gas (null), 'SGTN'
!   109 dissolved-aqueous oil concentration (kg/m^3), 'CLO '
!   110 salt aqueous mass fraction (null), 'XLS '
!   111 surfactant volumetric concentration (kg/m^3), 'CS '
!   112 surfactant aqueous concentration (kg/m^3), 'CLS '
!   113 surfactant aqueous mass fraction (null), 'XLS '
!       or gas N2 mass fraction (null), 'XGN'
!   114 x surfactant flux (kg/m^2 s), ' US '
!   115 y surfactant flux (kg/m^2 s), ' VS '
!   116 z surfactant flux (kg/m^2 s), ' WS '
!   117 xnc surfactant flux (kg/m^2 s), 'USNC'
!   118 ync surfactant flux (kg/m^2 s), 'VSNC'
!   119 znc surfactant flux (kg/m^2 s), 'WSNC'
!   120 x dissolved-oil flux (kg/m^2 s), 'ULO '
!   121 y dissolved-oil flux (kg/m^2 s), 'VLO '
!   122 z dissolved-oil flux (kg/m^2 s), 'WLO '
!   123 xnc dissolved-oil flux (kg/m^2 s), 'ULOC'
!   124 ync dissolved-oil flux (kg/m^2 s), 'VLOC'
!   125 znc dissolved-oil flux (kg/m^2 s), 'WLOC'
!   126 NAPL-aqueous trapping number (null), 'TPNL'
!   127 minimum effect aqueous saturation (null), 'ESLM'
!   128 water vapor partial pressure (Pa), 'PVW '
!   129 air partial pressure (Pa), 'PVA '
!   130 oil vapor partial pressure (Pa), 'PVO '
!   130 stress-zz (Pa), 'SIGZZ'
!   131 gas-aqueous scaling factor (null), 'BGL '
!   132 free-NAPL aqueous interfacial area (m^2), 'ANFL'
!   133 trapped-NAPL aqueous interfacial area (m^2), 'ANTL'
!   134 aqueous solute coefficient 'HKL '
!   135 free-NAPL solute coefficient 'HKNF'
!   136 trapped-NAPL solute coefficient 'HKNT'
!   137 water relative humidity (null), 'RHW '
!   138 well pressure (Pa), 'PLWB'
!       or coupled-well pressure (Pa), 'PCW'
!       or injection well pressure (Pa), 'IWP'
!   139 volumetric molar density (kg/m^3), 'RHMV'
!   140 water mass source rate (kg/s), 'SRCW'
!   141 air mass source rate (kg/s), 'SRCA'
!       or component mass source rate (kg/s), 'SRCC'
!   142 oil mass source rate (kg/s), 'SRCO'
!       or coupled-well CO2 mass nodal rate (kg/s), 'QNRA'
!   143 energy source rate (W), 'SRCQ'
!   144 aqueous well depth (m), 'PLWB'
!   145 well mass flow rate (kg/s), 'QLW '
!       or coupled-well water mass nodal rate (kg/s), 'QNRW'
!   146 well flow integral (kg), 'QLWI'
!   147 salt mass source rate (kg/s), 'SRCS'
!   148 salt mass source integral (kg), 'SRIS'
!   149 scanning path (null), 'PATH'
!   150 aqueous air or co2 saturation (null), 'DAPS'
!       or Webb matching point saturation (null), 'WMPS'
!   151 bubble void fraction (null), 'BVF '
!       or aqueous N2 mole fraction (null), 'XMLN'
!   152 bubble air mass fraction (null), 'XBA '
!       or aqueous N2 mass fraction (null), 'XLN'
!   153 mineralized co2 'MCO2 '
!   154 napl well flow rate 'QNW '
!   155 napl well flow integral 'QNWI'
!   156 total well flow rate 'QTW '
!   157 total well flow integral 'QTWI'
!   160 gas N2 mole fraction (null), 'XMGN'
!   161 NAPL dissolved water concentration 'CNW '
!   162 NAPL dissolved water mole fraction (null), 'XMNW'
!       or N2 nonaqueous liquid mole fraction (null), 'XMNN'
!   163 NAPL dissolved water mass fraction (null), 'XNW '
!       or water liquid-CO2 mass fraction (null), 'XNW '
!   164 NAPL dissolved oil concentration 'CNO '
!   165 NAPL dissolved oil mole fraction (null), 'XMNO'
!       or CH4 nonaqueous liquid mole fraction (null), 'XMNO'
!   166 NAPL dissolved oil mass fraction (null), 'XNO '
!       or CH4 liquid-CO2 mass fraction (null), 'XNO '
!   167 total alcohol mass 'TMA '
!   168 aqueous dissolved water mass fraction (null), 'XLW '
!   169 alcohol mass source integral 'SRIA'
!   170 alcohol mass source rate 'SRCA'
!   171 integrated NAPL and aqueous dissolved alcohol 'IMA '
!       or integrated kerogen mass 'IMK'
!       or integrated hydrate N2 mass 'IMHN'
!   172 integrated aqueous dissolved alcohol 'IMLA'
!       or integrated char mass 'IMCH'
!   173 integrated NAPL dissolved water 'IMNW'
!       or integrated coke mass 'IMCK'
!   174 integrated NAPL dissolved oil 'IMNO'
!   175 integrated NAPL dissolved alcohol 'IMNA'
!   176 aqueous viscosity 'VISL'
!   177 monitoring well water depth or total-liquid well depth 'WDT '
!   178 aqueous well depth 'WDL '
!   179 NAPL well depth 'WDN '
!   180 monitoring well pressure ' PW '
!       or coupled-well nodal pressure 'PNCW'
!   181 monitoring well aqueous saturation 'SLW '
!   182 monitoring well water-vapor mass fraction or well NAPL saturation 'XGWW'
!   183 monitoring well dissolved-air mass fraction or dissolved-oil mass fraction 'XLAW'
!   184 monitoring well axial aqueous flux or well total-liquid pumping rate 'UL_W'
!   185 monitoring well axial gas flux or well aqueous pumping rate 'UG_W'
!   186 monitoring well vertical aqueous flux or well NAPL pumping rate 'WL_W'
!   187 monitoring well vertical gas flux or well total-liquid pumping integral 'WG_W'
!   188 integrated well aqueous pumping 'IPLW'
!   189 integrated mineral CO2 or well NAPL pumping integral 'IPNW'
!   190 integrated trapped gas air 'IMGT'
!       or integrated N2 mass
!   191 integrated water mass (kg), 'IMW '
!   192 integrated air mass (kg), 'IMA '
!       or integrated petroleum component mass (kg), IMC
!   193 integrated oil mass (kg), 'IMO '
!   194 integrated aqueous water mass (kg), 'IMLW'
!   195 integrated aqueous air mass (kg), 'IMLA'
!   196 integrated aqueous oil mass (kg), 'IMLO'
!   197 integrated gas water mass (kg), 'IMGW'
!       or integrated volumetric component mass (kg), 'IMVC'
!   198 integrated gas air mass (kg), 'IMGA'
!       or integrated gas component mass (kg), 'IMGC'
!   199 integrated gas oil mass (kg), 'IMGO'
!       or integrated NAPL component mass (kg), 'IMNC'
!   200 reserved to control plot file output
!   201 x aqueous relative permeability (null), 'RKLX'
!   202 y aqueous relative permeability (null), 'RKLY'
!   203 z aqueous relative permeability (null), 'RKLZ'
!   204 aqueous co2 mole fraction (null), 'XMLA'
!       or aqueous component mole fraction (null), 'XMLC' 
!   205 aqueous salt mole fraction (null), 'XMLS'
!   206 atmospheric temperature (C), ' TA '
!       or critical temperature (C), ' TCR '
!   207 atmospheric relative humidity (null), ' RH '
!   208 atmospheric solar radiation ' RN '
!   209 atmospheric wind speed (m/s), ' WS '
!   210 residual NAPL saturation (null), 'SNR '
!   211 mobile NAPL saturation (null), 'SNM '
!   212 free NAPL saturation (null), 'SNF '
!   213 surface temperature (C), 'T_S'
!       or cricondentherm temperature (C), ' TCT '
!   214 surface vapor pressure (Pa), 'PV_S'
!   214 stress-yz (Pa), 'SIGYZ'
!   215 actual evaporation rate 'E_SA'
!   216 potential evaporation rate 'PE_SA'
!   217 actual transpiration rate 'T_SA'
!   218 potential transpiration rate 'PT_SA'
!   219 saturated co2 aqueous mass fraction 'SXLA'
!   220 aqueous alcohol mole fraction (null), 'XMLA'
!       or aqueous N2 mole fraction (null), 'XMLN'
!   221 NAPL alcohol mode fraction (null), 'XMNA'
!       or CO2 nonaqueous liquid mole fraction (null), 'XMNA'
!   222 aqueous alcohol mass fraction (null), 'XLA '
!       or nonaqueous N2 liquid mass fraction (null), 'XNN'
!   223 NAPL alcohol mass fraction (null), 'XNA '
!       or CO2 liquid-CO2 mass fraction (null), 'XNA '
!   224 atmospheric pressure (Pa), ' PA '
!   224 stress-xz (Pa), 'SIGXZ'
!   225 surface aqueous pressure (Pa), 'PL_S'
!   226 surface gas pressure (Pa), 'PG_S'
!   226 stress-xy (Pa), 'SIGXY'
!   227 surface aqueous saturation (null), 'SL_S
!   228 surface latent heat flux (W/m^2), 'QL_S'
!   229 surface sensible heat flux (W/m^2), 'QH_S'
!   230 surface net long-wave radiation, 'RL_S'
!   231 surface net short-wave radiation, 'RS_S'
!   232 surface ground heat flux, 'QG_S'
!   233 surface water mass balance (kg/s), 'WB_S'
!   234 plant temperature (C), 'T_P' or 'TPXX'
!   235 plant temperature (C), 'TPXX'
!   236 plant temperature (C), 'TPXX'
!   237 plant temperature (C), 'TPXX'
!   238 plant temperature (C), 'TPXX'
!   239 rainfall interception mass (kg), 'RFIM'
!       or integrated aqueous N2 mass (kg), 'IMLN'
!   240 sorbed oil mass, (kg oil), 'TSO '
!       or integrated gas N2 mass (kg), 'IMGN'
!   241 sorbed oil mass fraction (kg oil/kg soil), 'XSO '
!       or hydrate N2 mass fraction (null), 'XHN'
!   242 sorbed oil volumetric concentration (kg oil/m^3), 'CSO '
!   243 bare-soil aerodynamic resistance (s/m), 'RABS'
!   244 surface volumetric precipitation rate (m^3/s), 'PV_S'
!   245 surface mass precipitation rate (kg/s), 'PM_S'
!   246 atmospheric precipitation rate (kg/s), 'PM_A'
!       or production well petroleum component mass rate (kg/s), 'PWCR'
!   247 x-direction intrinsic permeability (m^2), ' UK '
!   247 matrix permeability (m^2), 'KM' (STOMP-OS)
!   248 y-direction intrinsic permeability (m^2), ' VK '
!   248 fracture permeability (m^2), 'FM' (STOMP-OS)
!   249 z-direction intrinsic permeability (m^2), ' WK '
!   250 hydrate water mass fraction (null), 'XHW '
!   251 hydrate CO2 mass fraction (null), 'XHA '
!   252 hydrate CH4 mass fraction (null), 'XHO '
!   253 hydrate density (kg/m^3), 'RHOH'
!       or production well fluid density (kg/m^3), 'PWFD'
!   254 hydrate saturation (null), ' SH '
!   255 hydrate pressure (Pa), ' PH '
!   256 integrated hydrate water mass (kg), 'IMHW'
!   257 integrated hydrate CO2 mass (kg), 'IMHA'
!       or production well petroleum comp. mass integral (kg), 'PWCI'
!   258 integrated hydrate CH4 mass (kg), 'IMHO'
!   259 integrated aqueous CH4 mass (kg), 'IMLO'
!   260 integrated gas CH4 mass (kg), 'IMGO'
!   261 integrated water mass source (kg), 'IMSW'
!   262 integrated air or CO2 mass source (kg), 'IMSA'
!   263 integrated oil or CH4 mass source (kg), 'IMSO'
!   264 precipitated salt saturation (null), ' SS '
!   265 hydrate water mole fraction (null), 'XMHW'
!   266 hydrate CO2 mole fraction (null), 'XMHA'
!   267 hydrate CH4 mole fraction (null), 'XMHO'
!   268 plant stomatal resistance (s/m), 'RS_P' or 'RSPX'
!   269 plant stomatal resistance (s/m), 'RSPX'
!   270 plant stomatal resistance (s/m), 'RSPX'
!   271 plant stomatal resistance (s/m), 'RSPX'
!   272 plant stomatal resistance (s/m), 'RSPX'
!   273 fracture adjacent index (null), 'FAI '
!   274 rain-water runoff volumetric rate (m^3/s), 'RWRO'
!   275 well-node pressure (Pa), 'WNP'
!       or MCStress (Pa), 'MCSTR'
!   276 well-node temperature (C), 'WNT'
!   277 differential integrated hydrate water mass (kg), 'DMHW'
!   278 differential integrated hydrate CO2 mass (kg), 'DMHA'
!   279 differential integrated hydrate CH4 mass (kg), 'DMHO'
!   280 differential integrated aqueous CH4 mass (kg), 'DMLO'
!       or differential integrated hydrate N2 mass (kg), 'DMHN'
!   281 differential integrated gas CH4 mass (kg), 'DMGO'
!       or differential integrated mobile water mass (kg), 'DMMW'
!   282 differential integrated water or water mass (kg), 'DMW'
!       or differential integrated mobile CO2 mass (kg), 'DMMA'
!   283 differential integrated air or CO2 mass (kg), 'DMA'
!       or differential integrated mobile CH4 mass (kg), 'DMMO'
!   284 differential integrated oil or CH4 mass (kg), 'DMO'
!       or differential integrated mobile N2 mass (kg), 'DMMN'
!   285 source-well pressure Pa, 'SWP'
!   286 source-well temperature C, 'SWT'
!   287 aqueous spill head/height m, 'HLSP'
!   288 NAPL spill head/height m, 'HNSP'
!   289 gas viscosity, 'VISG'
!   290 NAPL viscosity, 'VISN'
!   291 x node position, 'XP'
!   292 y node position, 'YP'
!   293 z node position, 'ZP'
!   294 gas CH4 mole fraction of formers, 'YMGO'
!   295 hydrate CH4 mole fraction of formers, 'YMHGO'
!   296 aqueous enthalpy, 'HL'
!   297 gas enthalpy, 'HG'
!   298 nonaqueous liquid phase enthalpy, 'HN'
!   299 similitude parameter (m^2/s), 'SIMV'
!   341 aqueous internal energy, 'UL'
!   342 gas internal energy, 'UG'
!   343 nonaqueous liquid phase internal energy, 'UN'
!   344 aqueous thermal conductivity, 'THKL'
!   345 gas thermal conductivity, 'THKG'
!   346 nonaqueous liquid phase thermal conductivity, 'THKN'
!   347 CO2 aqueous diffusion coefficient, 'DFLA' 
!   348 water gas diffusion coefficient, 'DFGW' 
!   349 coupled-well CO2 mass rate (kg/s), 'QMRA'
!       or injection well mass rate (kg/s), 'IWMR'
!       or production well total petroleum mass rate (kg/s), 'PWPR'
!   350 coupled-well CO2 mass integral (kg), 'QMIA'
!       or injection well mass integral (kg), 'IWMI'
!       or production well total petroleum mass integral (kg), 'PWPI'
!   351 coupled-well water mass rate (kg/s), 'QMRW'
!       or production well water mass rate (kg/s), 'PWWR'
!   352 coupled-well water mass integral (kg) 'QMIW'
!       or production well water mass integral (kg), PWWI
!   361 vertically-integrated CO2 mass (kg), VIA
!       or differential integrated N2 gas mass (kg), DMGN
!   362 vertically-integrated CO2 mass, (kg/m^2), VIAPA
!   363 vertically-integrated CO2 mass (kg), VIGA
!       or differential integrated water aqueous mass (kg), DMLW
!   364 vertically-integrated CO2 mass, kg/m^2, VIGAPA
!   365 vertically-integrated CO2 mass (kg), VILA
!   366 vertically-integrated CO2 mass, kg/m^2, VILAPA
!   367 integrated precipitated salt mass (kg), IMPS
!   368 mean effective stress, Pa, STRS-M
!   369 strain xx, EPSXX
!   370 strain yy, EPSYY
!   371 strain zz, EPSZZ
!   372 strain yz, EPSYZ
!   373 strain xz, EPSXZ
!   374 strain xy, EPSXY
!   375 x displacement, m, DISPX
!   376 y displacement, m, DISPY
!   377 z displacement, m, DISPZ
!   378 integrated energy, J, 'IQ'
!   379 gas CO2 mole fraction of formers, 'YMGA'
!   380 hydrate-gas CO2 mole fraction of former,s 'YMHGA'
!   381 gas N2 mole fraction of formers, 'YMGN'
!   382 hydrate-gas N2 mole fraction of formers, 'YMHGN'
!   383 total nonaqueous CO2 mole fraction of formers, 'ZMCA'
!       or total petroleum component mole fraction, 'ZMC'
!   384 total nonaqueous CH4 mole fraction of formers, 'ZMCO'
!   385 total nonaqueous N2 mole fraction of formers, 'ZMCN'
!   386 hydrate equilibrium pressure, 'PEQH'
!   387 total mobile formers vapor pressure, 'PVTF'
!   388 x-direction index of node, 'I'
!   389 y-direction index of node, 'J'
!   390 z-direction index of node, 'K'
!   391 xnc surface area, m^2 'AFX'
!   392 ync surface area, m^2 'AFY'
!   393 znc surface area, m^2 'AFZ'
!   394 saturation function index, 'SFZN'
!   395 processor id, 'PID'
!   401 solute volumetric concentration 'C   '
!   401 total_species volumetric concentration, mol/m^3, 'SP  '
!   402 solute aqueous concentration 'CL  '
!   402 total_species aqueous concentration 'SPL  '
!   403 solute gas concentration 'CG  '
!   403 total_species gas concentration 'SPG  '
!   404 solute NAPL concentration 'CN  '
!   404 total_species nonaqueus-liquid concentration 'SPN  '
!   405 solute aqueous mole fraction 'YL  '
!   406 solute gas mole fraction 'YG  '
!   407 solute NAPL mole fraction 'YN  '
!   408 x solute flux 'UC  '
!   409 y solute flux 'VC  '
!   410 z solute flux 'WC  '
!   411 solute source 'SRC '
!   411 total_species source 'SPSR'
!   412 exchange total_species volumetric concentration, mol/m^3, 'SPX '
!   413 exchange total_species aqueous concentration mol/m^3, 'SPLX'
!   414 exchange total_species gas concentration mol/m^3, 'SPGX'
!   415 exchange total_species nonaqueus-liquid concentration 'SPNX'
!   423 solute integrated mass 'ICM'
!   423 total_species integrated mass 'SPIM'
!   426 exchange total_species volumetric concentration, mol/m^3, 'SPS '
!   427 exchange total_species aqueous concentration mol/m^3, 'SPLS'
!   428 exchange total_species gas concentration mol/m^3, 'SPGS'
!   429 exchange total_species nonaqueus-liquid concentration 'SPNS'
!   430 solute mass concentration, 1/kg soil, 'CM'
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, February, 1993.




!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE TRNSPT
      USE SOLTN
      USE REACT
      USE PORMED
      USE OUTPU
      USE GRID
      USE FLUXS
      USE FLUXD
      USE FILES
      USE FDVP
      USE CONST
!

!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Parameter Statements----------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*4 FORM1
      CHARACTER*16 FN,FORM2
      CHARACTER*11 FORM3
      CHARACTER*64 SPNMX
      REAL*8, DIMENSION(:), ALLOCATABLE ::  DVAR
      INTEGER, DIMENSION(:), ALLOCATABLE ::  IVAR
      INTEGER MXZ(4),MXY(4),MYZ(4),MX(2),MY(2),MZ(2)
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Data Statements---------------------------------!
!
      SAVE FORM1,FORM2,FORM3
      SAVE MXZ,MXY,MYZ,MX,MY,MZ
      DATA FORM1 / '(I )' /
      DATA FORM2 / '(10(1PE16.9,1X))' /
      DATA FORM3 / '(10(I2,1X))' /
      DATA MXZ / 1,2,5,6 /
      DATA MXY / 1,2,3,4 /
      DATA MYZ / 1,3,5,7 /
      DATA MX / 1,2 /
      DATA MY / 1,3 /
      DATA MZ / 1,5 /
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/WRPLOT'
      IF( INDEX(SVN_ID(277)(1:1),'$').EQ.0 ) SVN_ID(277) =
     & '$Id: wrplot.F 1080 2017-03-14 16:22:02Z d3c002 $' 
      WRITE(FORM2(8:9),'(I2)') MAX( 10,ISGNP+6 )
      WRITE(FORM2(11:11),'(I1)') MIN( 9,ISGNP-1 )
!
!---  Dynamic memory allocation  ---
!
      ALLOCATE( DVAR(1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DVAR'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IVAR(1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IVAR'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Create a new plot file with number of time steps as the file name
!     extension  ---
!
      FN(1:5) = 'plot.'
      N1X = MIN( ICOUNT( MXSTEP+NRST ),9 )
      DO 10 N2X = 1,N1X
        N3X = N2X + 5
        FN(N3X:N3X) = '0'
   10 CONTINUE
      N4X = ICOUNT( NSTEP )
      WRITE(FORM1(3:3),'(I1)') N4X
      N5X = 6 + N1X - N4X
      WRITE( FN(N5X:),FORM1) NSTEP
      OPEN(UNIT=IPL, FILE=FN, STATUS='UNKNOWN', FORM='FORMATTED')
      CLOSE(UNIT=IPL, STATUS='DELETE')
      OPEN(UNIT=IPL, FILE=FN, STATUS='NEW', FORM='FORMATTED')
!
!---  Write header  ---
!
      WRITE(IPL,'(A,//)')' Welcome to ...'
      WRITE(IPL,'(A)')   '                           STOMP'
      WRITE(IPL,'(A,//)')'        Subsurface Transport Over ' // 
     &  'Multiple Phases'
      WRITE(IPL,'(A)')   ' This file was produced by STOMP, ' //
     &  'a numerical simulator'
      WRITE(IPL,'(A)')   ' developed by the Pacific Northwest ' //
     &  'Laboratory, with'
      WRITE(IPL,'(A)')   ' support from the VOC-Arid Integrated ' //
     &  'Demonstration Project,'
      WRITE(IPL,'(A)')   ' Office of Technology Development, ' //
     &  'U.S. Department of Energy.'
      WRITE(IPL,'(A)')   ' Results from this version of STOMP ' //
     &  'should not be used for'
      WRITE(IPL,'(A,/)') ' license related applications.'
      WRITE(IPL,'(A,/)') ' For inquiries or assistance:  ' //
     &  'Call (509) 372-6070'
      WRITE(IPL,'(A,//)')'                        ---  PLOT  ---'

      WRITE(IPL,'(2A)') 'Version: ',CH_VRSN




      WRITE(IPL,'(2A)') ' Date: ','Date system call inactive'
      WRITE(IPL,'(2A,//)') ' Time: ','Time system call inactive'
!      WRITE(IWR,'(2A)') ' Date: ',CHDATE
!      WRITE(IWR,'(2A,//)') ' Time: ',CHTIME

!
!---  Write field data by node numbers
!     write flux data by surface numbers  ---
!
      WRITE(IPL,'(/,A)') '--- Field Variable Data by Node Numbers'
      WRITE(IPL,'(A)') '    Flux Variable Data by Surface Numbers  ---'
      WRITE(IPL,'(/,A,I9)') 'Number of Time Steps = ',NSTEP
      TMIN = TM/60.
      THR = TM/3600.
      TDAY = THR/24.
      TWK = TDAY/7.
      TYR = TDAY/365.2425D+0






      WRITE(IPL,'(A,6(1PE13.6,A))') 'Time = ',TM,',s ',TMIN,',min ',THR,
     &',h ',TDAY,',day ',TWK,',wk ',TYR,',yr '

!
!---  Reference depth of grid  ---
!
      VAR = 1.D+0
      INDX = 4
      IUNM = 1
      CALL RDUNIT(UNLN,VAR,INDX)
      IDB = INDEX( UNLN(1:),'  ') - 1
      IF( IXREF(3).EQ.1 ) THEN
        WRITE(IPL,'(/,2A,1PE13.6,A)') 'Reference Depth of Grid, ',
     &    UNLN(1:IDB),' = ',(VAR*XREF(3))
      ENDIF
      WRITE(IPL,'(/,A,I9)') 'Number of X or R-Direction Nodes = ',IFLD
      WRITE(IPL,'(A,I9)') 'Number of Y or Theta-Direction Nodes = ',JFLD
      WRITE(IPL,'(A,I9)') 'Number of Z-Direction Nodes = ',KFLD
      WRITE(IPL,'(A,I9)') 'Number of Field Nodes = ',NFLD
      IF( NFBN.GT.NFLD ) THEN
        WRITE(IPL,'(A,I9)') 'Number of Block Refinement Nodes = ',
     &    (NFBN-NFLD)
        WRITE(IPL,'(A,I9)') 'Number of Field and Block Refinement ' //
     &    'Nodes = ',NFBN
            WRITE(IPL,'(A,I9)') 'Number of Refined Field Nodes = ',NRFN
        WRITE(IPL,'(A,I9)') 'Number of Active Nodes = ',(NFBN-NRFN-NXP)
        NFX = NFBN
      ELSE
        NFX = NFLD
        WRITE(IPL,'(A,I9)') 'Number of Active Nodes = ',(NFLD-NXP)
      ENDIF
!
!---  X-Direction vertices, 1 node per line, 8 vertices per line  ---
!
!
!---  Cylindrical coordinates  ---
!
      IF( ICS.EQ.2 ) THEN
        IF( (IFLD.GT.1 .AND. JFLD.GT.1 .AND. KFLD.GT.1) .OR.
     &    IJFLD.EQ.1 .OR. JKFLD.EQ.1 .OR. KIFLD.EQ.1 .OR. 
     &    ISLC(63).EQ.1 ) THEN
          WRITE(IPL,'(A)') 'Number of Vertices = 8'
          WRITE(IPL,'(/,2A)') 'X-Direction Nodal Vertices, ',UNLN(1:IDB)
          DO 100 N = 1,NFX
            WRITE(IPL,'(8(1PE16.9,1X))')
     &        ((VAR*XE(M,N)*COS(YE(M,N))),M=1,8)
 100     CONTINUE
        ELSEIF( IFLD.GT.1 .AND. JFLD.GT.1 ) THEN
          WRITE(IPL,'(A)') 'Number of Vertices = 4'
          WRITE(IPL,'(/,2A)') 'X-Direction Nodal Vertices, ',UNLN(1:IDB)
          DO 102 N = 1,NFX
            WRITE(IPL,'(4(1PE16.9,1X))')
     &        ((VAR*XE(MXY(M),N)*COS(YE(MXY(M),N))),M=1,4)
 102     CONTINUE
        ELSEIF( IFLD.GT.1 .AND. KFLD.GT.1 ) THEN
          WRITE(IPL,'(A)') 'Number of Vertices = 4'
          WRITE(IPL,'(/,2A)') 'X-Direction Nodal Vertices, ',UNLN(1:IDB)
          DO 104 N = 1,NFX
            WRITE(IPL,'(4(1PE16.9,1X))')
     &        ((VAR*XE(MXZ(M),N)*COS(YE(MXZ(M),N))),M=1,4)
 104     CONTINUE
        ELSEIF( IFLD.GT.1 ) THEN
          WRITE(IPL,'(A)') 'Number of Vertices = 2'
          WRITE(IPL,'(/,2A)') 'X-Direction Nodal Vertices, ',UNLN(1:IDB)
          DO 106 N = 1,NFX
            WRITE(IPL,'(2(1PE16.9,1X))')
     &        ((VAR*XE(MX(M),N)*COS(YE(MX(M),N))),M=1,2)
 106     CONTINUE
        ENDIF
      ELSE
        IF( (IFLD.GT.1 .AND. JFLD.GT.1 .AND. KFLD.GT.1) .OR.
     &    ICS.EQ.3 .OR. ISLC(63).EQ.1 .OR.
     &    IJFLD.EQ.1 .OR. JKFLD.EQ.1 .OR. KIFLD.EQ.1 ) THEN
          WRITE(IPL,'(A)') 'Number of Vertices = 8'
          WRITE(IPL,'(/,2A)') 'X-Direction Nodal Vertices, ',UNLN(1:IDB)
          DO 110 N = 1,NFX
            WRITE(IPL,'(8(1PE16.9,1X))')
     &        ((VAR*XE(M,N)),M=1,8)
 110     CONTINUE
        ELSEIF( IFLD.GT.1 .AND. JFLD.GT.1 ) THEN
          WRITE(IPL,'(A)') 'Number of Vertices = 4'
          WRITE(IPL,'(/,2A)') 'X-Direction Nodal Vertices, ',UNLN(1:IDB)
          DO 112 N = 1,NFX
            WRITE(IPL,'(4(1PE16.9,1X))')
     &        ((VAR*XE(MXY(M),N)),M=1,4)
 112     CONTINUE
        ELSEIF( IFLD.GT.1 .AND. KFLD.GT.1 ) THEN
          WRITE(IPL,'(A)') 'Number of Vertices = 4'
          WRITE(IPL,'(/,2A)') 'X-Direction Nodal Vertices, ',UNLN(1:IDB)
          DO 114 N = 1,NFX
            WRITE(IPL,'(4(1PE16.9,1X))')
     &        ((VAR*XE(MXZ(M),N)),M=1,4)
 114     CONTINUE
        ELSEIF( IFLD.GT.1 ) THEN
          WRITE(IPL,'(A)') 'Number of Vertices = 2'
          WRITE(IPL,'(/,2A)') 'X-Direction Nodal Vertices, ',UNLN(1:IDB)
          DO 116 N = 1,NFX
            WRITE(IPL,'(2(1PE16.9,1X))')
     &        ((VAR*XE(MX(M),N)),M=1,2)
 116     CONTINUE
        ENDIF
      ENDIF
!
!---  Y-Direction vertices, 1 node per line, 8 vertices per line  ---
!
!
!---  Cylindrical coordinates  ---
!
      IF( ICS.EQ.2 ) THEN
        IF( IFLD.GT.1 .AND. JFLD.GT.1 .AND. KFLD.GT.1 .OR.
     &    IJFLD.EQ.1 .OR. JKFLD.EQ.1 .OR. KIFLD.EQ.1 .OR. 
     &    ISLC(63).EQ.1 ) THEN
          WRITE(IPL,'(/,2A)') 'Y-Direction Nodal Vertices, ',UNLN(1:IDB)
          DO 120 N = 1,NFX
            WRITE(IPL,'(8(1PE16.9,1X))')
     &        ((VAR*XE(M,N)*SIN(YE(M,N))),M=1,8)
 120     CONTINUE
        ELSEIF( IFLD.GT.1 .AND. JFLD.GT.1 ) THEN
          WRITE(IPL,'(/,2A)') 'Y-Direction Nodal Vertices, ',UNLN(1:IDB)
          DO 122 N = 1,NFX
            WRITE(IPL,'(4(1PE16.9,1X))')
     &        ((VAR*XE(MXY(M),N)*SIN(YE(MXY(M),N))),M=1,4)
 122     CONTINUE
        ELSEIF( JFLD.GT.1 .AND. KFLD.GT.1 ) THEN
          WRITE(IPL,'(A)') 'Number of Vertices = 4'
          WRITE(IPL,'(/,2A)') 'Y-Direction Nodal Vertices, ',UNLN(1:IDB)
          DO 124 N = 1,NFX
            WRITE(IPL,'(4(1PE16.9,1X))')
     &        ((VAR*XE(MYZ(M),N)*SIN(YE(MYZ(M),N))),M=1,4)
 124     CONTINUE
        ELSEIF( JFLD.GT.1 ) THEN
          WRITE(IPL,'(A)') 'Number of Vertices = 2'
          WRITE(IPL,'(/,2A)') 'Y-Direction Nodal Vertices, ',UNLN(1:IDB)
          DO 126 N = 1,NFX
            WRITE(IPL,'(2(1PE16.9,1X))')
     &        ((VAR*XE(MY(M),N)*SIN(YE(MY(M),N))),M=1,2)
 126     CONTINUE
        ENDIF
      ELSE
        IF( IFLD.GT.1 .AND. JFLD.GT.1 .AND. KFLD.GT.1 .OR.
     &    ICS.EQ.3 .OR. ISLC(63).EQ.1 .OR.
     &    IJFLD.EQ.1 .OR. JKFLD.EQ.1 .OR. KIFLD.EQ.1 ) THEN
          WRITE(IPL,'(/,2A)') 'Y-Direction Nodal Vertices, ',UNLN(1:IDB)
          DO 130 N = 1,NFX
            WRITE(IPL,'(8(1PE16.9,1X))')
     &        ((VAR*YE(M,N)),M=1,8)
 130     CONTINUE
        ELSEIF( IFLD.GT.1 .AND. JFLD.GT.1 ) THEN
          WRITE(IPL,'(/,2A)') 'Y-Direction Nodal Vertices, ',UNLN(1:IDB)
          DO 132 N = 1,NFX
            WRITE(IPL,'(4(1PE16.9,1X))')
     &        ((VAR*YE(MXY(M),N)),M=1,4)
 132     CONTINUE
        ELSEIF( JFLD.GT.1 .AND. KFLD.GT.1 ) THEN
          WRITE(IPL,'(A)') 'Number of Vertices = 4'
          WRITE(IPL,'(/,2A)') 'Y-Direction Nodal Vertices, ',UNLN(1:IDB)
          DO 134 N = 1,NFX
            WRITE(IPL,'(4(1PE16.9,1X))')
     &        ((VAR*YE(MYZ(M),N)),M=1,4)
 134     CONTINUE
        ELSEIF( JFLD.GT.1 ) THEN
          WRITE(IPL,'(A)') 'Number of Vertices = 2'
          WRITE(IPL,'(/,2A)') 'Y-Direction Nodal Vertices, ',UNLN(1:IDB)
          DO 136 N = 1,NFX
            WRITE(IPL,'(2(1PE16.9,1X))')
     &        ((VAR*YE(MY(M),N)),M=1,2)
 136     CONTINUE
        ENDIF
      ENDIF
!
!---  Z-Direction vertices, 1 node per line, 8 vertices per line  ---
!
      IF( IFLD.GT.1 .AND. JFLD.GT.1 .AND. KFLD.GT.1 .OR.
     &    ICS.EQ.3 .OR. ISLC(63).EQ.1 .OR.
     &    IJFLD.EQ.1 .OR. JKFLD.EQ.1 .OR. KIFLD.EQ.1 ) THEN
        WRITE(IPL,'(/,2A)') 'Z-Direction Nodal Vertices, ',UNLN(1:IDB)
        DO 140 N = 1,NFX
          WRITE(IPL,'(8(1PE16.9,1X))')
     &      ((VAR*ZE(M,N)),M=1,8)
 140   CONTINUE
      ELSEIF( IFLD.GT.1 .AND. KFLD.GT.1 ) THEN
        WRITE(IPL,'(/,2A)') 'Z-Direction Nodal Vertices, ',UNLN(1:IDB)
        DO 142 N = 1,NFX
          WRITE(IPL,'(4(1PE16.9,1X))')
     &      ((VAR*ZE(MXZ(M),N)),M=1,4)
 142   CONTINUE
      ELSEIF( JFLD.GT.1 .AND. KFLD.GT.1 ) THEN
        WRITE(IPL,'(/,2A)') 'Z-Direction Nodal Vertices, ',UNLN(1:IDB)
        DO 144 N = 1,NFX
          WRITE(IPL,'(4(1PE16.9,1X))')
     &      ((VAR*ZE(MYZ(M),N)),M=1,4)
 144   CONTINUE
      ELSEIF( KFLD.GT.1 ) THEN
          WRITE(IPL,'(A)') 'Number of Vertices = 2'
        WRITE(IPL,'(/,2A)') 'Z-Direction Nodal Vertices, ',UNLN(1:IDB)
        DO 146 N = 1,NFX
          WRITE(IPL,'(2(1PE16.9,1X))')
     &      ((VAR*ZE(MZ(M),N)),M=1,2)
 146   CONTINUE
      ENDIF
      IDB = INDEX( UNLN(1:),'  ') - 1
!
!---  Node volume  ---
!
      WRITE(IPL,'(/,3A)') 'Node Volume, ',UNLN(1:IDB),'^3'
      WRITE(IPL,FORM2) ((VAR**3)*VOL(N),N=1,NFX)
!
!---  Node map  ---
!
      WRITE(FORM3(6:6),'(I1)') ICOUNT(NFX)+1
      WRITE(IPL,'(/,A)') 'Node Map'
      DO N = 1,NFX
        IF( IXP(N).LE.0 ) THEN
          IVAR(N) = 0
        ELSE
          IVAR(N) = IXP(N)
        ENDIF
      ENDDO
      WRITE(IPL,FORM3) (IVAR(N),N=1,NFX)
!
!---  Plot variables  ---
!
      DO 500 NV = 1,NVPLOT
        VAR = 1.D+0
        IPNV = IPLOT(NV)
        IPNVGC = IPLOTGC(NV)
        IF( IPNV.LE.16 ) THEN
          CALL WRPL_1( FORM2,IPNV,IPNVGC,NFX )
        ELSEIF( IPNV.GT.16 .AND. IPNV.LE.32 ) THEN
          CALL WRPL_2( FORM2,IPNV,IPNVGC,NFX )
        ELSEIF( IPNV.GT.32 .AND. IPNV.LE.48 ) THEN
          CALL WRPL_3( FORM2,IPNV,IPNVGC,NFX )
        ELSEIF( IPNV.GT.48 .AND. IPNV.LE.64 ) THEN
          CALL WRPL_4( FORM2,IPNV,IPNVGC,NFX )
        ELSEIF( IPNV.GT.64 .AND. IPNV.LE.80 ) THEN
          CALL WRPL_5( FORM2,IPNV,IPNVGC,NFX )
        ELSEIF( IPNV.GT.80 .AND. IPNV.LE.96 ) THEN
          CALL WRPL_6( FORM2,IPNV,IPNVGC,NFX )
        ELSEIF( IPNV.GT.96 .AND. IPNV.LE.112 ) THEN
          CALL WRPL_7( FORM2,IPNV,IPNVGC,NFX )
        ELSEIF( IPNV.GT.112 .AND. IPNV.LE.128 ) THEN
          CALL WRPL_8( FORM2,IPNV,IPNVGC,NFX )
        ELSEIF( IPNV.GT.128 .AND. IPNV.LE.144 ) THEN
          CALL WRPL_9( FORM2,IPNV,IPNVGC,NFX )
        ELSEIF( IPNV.GT.144 .AND. IPNV.LE.160 ) THEN
          CALL WRPL_10( FORM2,IPNV,IPNVGC,NFX )
        ELSEIF( IPNV.GT.160 .AND. IPNV.LE.176 ) THEN
          CALL WRPL_11( FORM2,IPNV,IPNVGC,NFX )
        ELSEIF( IPNV.GT.176 .AND. IPNV.LE.192 ) THEN
          CALL WRPL_12( FORM2,IPNV,IPNVGC,NFX )
        ELSEIF( IPNV.GT.192 .AND. IPNV.LE.208 ) THEN
          CALL WRPL_13( FORM2,IPNV,IPNVGC,NFX )
        ELSEIF( IPNV.GT.208 .AND. IPNV.LE.224 ) THEN
          CALL WRPL_14( FORM2,IPNV,IPNVGC,NFX )
        ELSEIF( IPNV.GT.224 .AND. IPNV.LE.240 ) THEN
          CALL WRPL_15( FORM2,IPNV,IPNVGC,NFX )
        ELSEIF( IPNV.GT.240 .AND. IPNV.LE.256 ) THEN
          CALL WRPL_16( FORM2,IPNV,IPNVGC,NFX )
        ELSEIF( IPNV.GT.256 .AND. IPNV.LE.272 ) THEN
          CALL WRPL_17( FORM2,IPNV,IPNVGC,NFX )
        ELSEIF( IPNV.GT.273 .AND. IPNV.LE.299 ) THEN
          CALL WRPL_18( FORM2,IPNV,IPNVGC,NFX )
        ELSEIF( IPNV.GT.340 .AND. IPNV.LE.400 ) THEN
          CALL WRPL_20( FORM2,IPNV,IPNVGC,NFX )
        ENDIF
!
!---    Solute, conservation-component species, and
!       kinetic-component species plot output ---
!
        INDX = 400+(NSOLU*33)+((NEQC+NEQK)*33)
        IF( IPNV.GT.400 .AND. IPNV.LE.INDX ) THEN
          NSL = ((IPNV-400)/33) + 1
          IF( NSL.GT.NSOLU ) IUNMOL = 1
          IPNVX = MOD((IPNV-400),33)
          IDB = INDEX( SOLUT(NSL)(1:),'  ') - 1
!
!---      Volumetric concentration  ---
!
          IF( IPNVX.EQ.1 ) THEN
            INDX = 4
            IUNM = -3
            CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
            IF( NSL.GT.NSOLU )THEN
              NEQ = NSL-NSOLU
              VAR = VAR*1.D-3
              DO 152 N = 1,NFX
                DVAR(N) = 0.D+0
                DO 150 M = 1,IEQ_C(1,NEQ)
                  NSP = IEQ_C(M+1,NEQ)
                  IF( NSP.LE.NSPL ) THEN
                    IF( ABS(SP_C(N,NSP)).LT.1.D-30 ) THEN
                      SP_CX = 0.D+0
                    ELSE
                      SP_CX = SP_C(N,NSP)
                    ENDIF
                    DVAR(N) = DVAR(N) + EQ_C(M,NEQ)*SP_CX
                  ENDIF
  150           CONTINUE
  152         CONTINUE
              WRITE(IPL,'(/,4A)') 'Volumetric ',SOLUT(NSL)(1:IDB), 
     &          ' Concentration, ',UNPLOT(IPNV)
              WRITE(IPL,FORM2) (VAR*DVAR(N),N=1,NFX)
            ELSE
              WRITE(IPL,'(/,4A)') 'Volumetric ',SOLUT(NSL)(1:IDB),
     &          ' Concentration, ',UNPLOT(IPNV)
              WRITE(IPL,FORM2) (VAR*C(N,NSL),N=1,NFX)
            ENDIF
!
!---      Aqueous concentration  ---
!
          ELSEIF( IPNVX.EQ.2 ) THEN
            INDX = 4
            IUNM = -3
            CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
            IF( NSL.GT.NSOLU ) THEN
              NEQ = NSL-NSOLU
              VAR = VAR*1.D-3
              DO 162 N = 1,NFX
                DVAR(N) = 0.D+0
                DO 160 M = 1,IEQ_C(1,NEQ)
                  NSP = IEQ_C(M+1,NEQ)
                  IF( NSP.LE.NSPL ) THEN
                    IF( ABS(SP_C(N,NSP)).LT.1.D-30 ) THEN
                      SP_CX = 0.D+0
                    ELSE
                      SP_CX = SP_C(N,NSP)
                    ENDIF
                    DVAR(N) = DVAR(N) + EQ_C(M,NEQ)*SP_CX
                  ENDIF
  160           CONTINUE
  162         CONTINUE
              WRITE(IPL,'(/,4A)') 'Aqueous ',
     &          SOLUT(NSL)(1:IDB), ' Concentration, ',UNPLOT(IPNV)
              WRITE(IPL,FORM2) ( VAR*DVAR(N)*YL(N,NSL)/
     &          (SL(2,N)*PORD(2,N)+SMALL),N=1,NFX )
            ELSE
              WRITE(IPL,'(/,4A)') 'Aqueous ',
     &          SOLUT(NSL)(1:IDB), ' Concentration, ',UNPLOT(IPNV)
              WRITE(IPL,FORM2) (VAR*C(N,NSL)*YL(N,NSL)/
     &          (SL(2,N)*PORD(2,N)+SMALL),N=1,NFX )
            ENDIF
!
!---      Gas concentration  ---
!
          ELSEIF( IPNVX.EQ.3 ) THEN
            INDX = 4
            IUNM = -3
            CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
            IF( NSL.GT.NSOLU ) THEN
              NEQ = NSL-NSOLU
              VAR = VAR*1.D-3
              DO 172 N = 1,NFX
                DVAR(N) = 0.D+0
                DO 170 M = 1,IEQ_C(1,NEQ)
                  NSP = IEQ_C(M+1,NEQ)
                  IF( NSP.LE.NSPL ) THEN
                    IF( ABS(SP_C(N,NSP)).LT.1.D-30 ) THEN
                      SP_CX = 0.D+0
                    ELSE
                      SP_CX = SP_C(N,NSP)
                    ENDIF
                    DVAR(N) = DVAR(N) + EQ_C(M,NEQ)*SP_CX
                  ENDIF
  170           CONTINUE
  172         CONTINUE
              WRITE(IPL,'(/,4A)') 'Gas ',SOLUT(NSL)(1:IDB),
     &          ' Concentration, ',UNPLOT(IPNV)
              WRITE(IPL,FORM2) ( VAR*DVAR(N)*YG(N,NSL)/
     &          (SG(2,N)*PORD(2,N)+SMALL),N=1,NFX )
            ELSE
              WRITE(IPL,'(/,4A)') 'Gas ',SOLUT(NSL)(1:IDB),
     &          ' Concentration, ',UNPLOT(IPNV)
              WRITE(IPL,FORM2) (VAR*C(N,NSL)*YG(N,NSL)/
     &          (SG(2,N)*PORD(2,N)+SMALL),N=1,NFX )
            ENDIF
!
!---      Nonaqueous-liquid concentration  ---
!
          ELSEIF( IPNVX.EQ.4 ) THEN
            INDX = 4
            IUNM = -3
            CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
            IF( NSL.GT.NSOLU ) THEN
              NEQ = NSL-NSOLU
              VAR = VAR*1.D-3
              DO 182 N = 1,NFX
                DVAR(N) = 0.D+0
                DO 180 M = 1,IEQ_C(1,NEQ)
                  NSP = IEQ_C(M+1,NEQ)
                  IF( NSP.LE.NSPL ) THEN
                    IF( ABS(SP_C(N,NSP)).LT.1.D-30 ) THEN
                      SP_CX = 0.D+0
                    ELSE
                      SP_CX = SP_C(N,NSP)
                    ENDIF
                    DVAR(N) = DVAR(N) + EQ_C(M,NEQ)*SP_CX
                  ENDIF
  180           CONTINUE
  182         CONTINUE
              WRITE(IPL,'(/,4A)') 'Nonaqueous-Liquid',
     &          SOLUT(NSL)(1:IDB),' Concentration, ',UNPLOT(IPNV)
              WRITE(IPL,FORM2) ( VAR*DVAR(N)*YN(N,NSL)/
     &          (SN(2,N)*PORD(2,N)+SMALL),N=1,NFX )
            ELSE
              WRITE(IPL,'(/,4A)') 'Nonaqueous-Liquid',
     &          SOLUT(NSL)(1:IDB),' Concentration, ',UNPLOT(IPNV)
              WRITE(IPL,FORM2) (VAR*C(N,NSL)*YN(N,NSL)/
     &          (SN(2,N)*PORD(2,N)+SMALL),N=1,NFX )
            ENDIF
          ELSEIF( IPNVX.EQ.5 ) THEN
            WRITE(IPL,'(/,3A)') 'Mole Fraction of ',SOLUT(NSL)(1:IDB),
     &        ' in the Aqueous Phase, '
            WRITE(IPL,FORM2) (YL(N,NSL),N=1,NFX)
          ELSEIF( IPNVX.EQ.6 ) THEN
            WRITE(IPL,'(/,3A)') 'Mole Fraction of ',SOLUT(NSL)(1:IDB),
     &        ' in the Gas Phase, '
            WRITE(IPL,FORM2) (YG(N,NSL),N=1,NFX)
          ELSEIF( IPNVX.EQ.7 ) THEN
            IF( IOM.EQ.1 ) THEN
              WRITE(IPL,'(/,3A)') 'Solute Inventory of ',SOLUT(NSL)
     &          (1:IDB)
            ELSE
              WRITE(IPL,'(/,3A)') 'Mole Fraction of ',SOLUT(NSL)(1:IDB),
     &          ' in the NAPL, '
            ENDIF
            WRITE(IPL,FORM2) (YN(N,NSL),N=1,NFX)
          ELSEIF( IPNVX.EQ.11 ) THEN
            INDX = 4
            CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
            WRITE(IPL,'(/,4A)') 'Source Integral of ',SOLUT(NSL)(1:IDB),
     &        ', ',UNPLOT(IPNV)
            WRITE(IPL,FORM2) (VAR*SRCIC(N,NSL),N=1,NFX)
          ELSEIF( IPNVX.EQ.12 ) THEN
            INDX = 4
            IUNM = -3
            CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
            IF( NSL.GT.NSOLU )THEN
              NEQ = NSL-NSOLU
              VAR = VAR*1.D-3
              DO 192 N = 1,NFX
                DVAR(N) = 0.D+0
                DO 190 M = 1,IEQ_C(1,NEQ)
                 NSP = IEQ_C(M+1,NEQ)
                 IF( NSP.GT.NSPL+NSPS .AND. NSP.LE.NSPL+NSPS+NSPE ) THEN
                   IF( ABS(SP_C(N,NSP)).LT.1.D-30 ) THEN
                     SP_CX = 0.D+0
                   ELSE
                     SP_CX = SP_C(N,NSP)
                   ENDIF
                   DVAR(N) = DVAR(N) + EQ_C(M,NEQ)*SP_CX
                 ENDIF
  190           CONTINUE
  192         CONTINUE
              WRITE(IPL,'(/,4A)') 'Exchange Volumetric ', 
     &          SOLUT(NSL)(1:IDB),' Concentration, ',UNPLOT(IPNV)
              WRITE(IPL,FORM2) (VAR*DVAR(N),N=1,NFX)
            ELSE
              WRITE(IPL,'(/,3A)') SOLUT(NSL)(1:IDB),
     &          ' Total Concentration, ',UNPLOT(IPNV)
              DO 194 N = 1,NFX
                DVAR(N) = YL(N,NSL)+YN(N,NSL)+YG(N,NSL)
  194         CONTINUE
              WRITE(IPL,FORM2) (VAR*DVAR(N),N=1,NFX)
            ENDIF
          ELSEIF( IPNVX.EQ.13 ) THEN
            INDX = 4
            IUNM = -3
            CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
            IF( NSL.GT.NSOLU )THEN
              NEQ = NSL-NSOLU
              VAR = VAR*1.D-3
              DO 202 N = 1,NFX
                DVAR(N) = 0.D+0
                DO 200 M = 1,IEQ_C(1,NEQ)
                 NSP = IEQ_C(M+1,NEQ)
                 IF( NSP.GT.NSPL+NSPS .AND. NSP.LE.NSPL+NSPS+NSPE ) THEN
                   IF( ABS(SP_C(N,NSP)).LT.1.D-30 ) THEN
                     SP_CX = 0.D+0
                   ELSE
                     SP_CX = SP_C(N,NSP)
                   ENDIF
                   DVAR(N) = DVAR(N) + EQ_C(M,NEQ)*SP_CX
                 ENDIF
  200           CONTINUE
  202         CONTINUE
              WRITE(IPL,'(/,4A)') 'Exchange Aqueous ', 
     &          SOLUT(NSL)(1:IDB),' Concentration, ',UNPLOT(IPNV)
              WRITE(IPL,FORM2) ( VAR*DVAR(N)*YL(N,NSL)/
     &          (SL(2,N)*PORD(2,N)+SMALL),N=1,NFX )
            ELSE
              WRITE(IPL,'(/,3A)') SOLUT(NSL)(1:IDB),
     &          ' Aqueous Concentration, ',UNPLOT(IPNV)
              WRITE(IPL,FORM2) (VAR*C(N,NSL),N=1,NFX)
            ENDIF
          ELSEIF( IPNVX.EQ.14 ) THEN
            INDX = 4
            IUNM = -3
            CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
            IF( NSL.GT.NSOLU )THEN
              NEQ = NSL-NSOLU
              VAR = VAR*1.D-3
              DO 212 N = 1,NFX
                DVAR(N) = 0.D+0
                DO 210 M = 1,IEQ_C(1,NEQ)
                 NSP = IEQ_C(M+1,NEQ)
                 IF( NSP.GT.NSPL+NSPS .AND. NSP.LE.NSPL+NSPS+NSPE ) THEN
                   IF( ABS(SP_C(N,NSP)).LT.1.D-30 ) THEN
                     SP_CX = 0.D+0
                   ELSE
                     SP_CX = SP_C(N,NSP)
                   ENDIF
                   DVAR(N) = DVAR(N) + EQ_C(M,NEQ)*SP_CX
                 ENDIF
  210           CONTINUE
  212         CONTINUE
              WRITE(IPL,'(/,4A)') 'Exchange Gas ', 
     &          SOLUT(NSL)(1:IDB),' Concentration, ',UNPLOT(IPNV)
              WRITE(IPL,FORM2) ( VAR*DVAR(N)*YG(N,NSL)/
     &          (SG(2,N)*PORD(2,N)+SMALL),N=1,NFX )
            ELSE
              WRITE(IPL,'(/,3A)') SOLUT(NSL)(1:IDB),
     &        ' NAPL Concentration, ',UNPLOT(IPNV)
              WRITE(IPL,FORM2) (VAR*CNL(N,NSL),N=1,NFX)
            ENDIF
          ELSEIF( IPNVX.EQ.15 ) THEN
            INDX = 4
            IUNM = -3
            CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
            IF( NSL.GT.NSOLU )THEN
              NEQ = NSL-NSOLU
              VAR = VAR*1.D-3
              DO 222 N = 1,NFX
                DVAR(N) = 0.D+0
                DO 220 M = 1,IEQ_C(1,NEQ)
                 NSP = IEQ_C(M+1,NEQ)
                 IF( NSP.GT.NSPL+NSPS .AND. NSP.LE.NSPL+NSPS+NSPE ) THEN
                   IF( ABS(SP_C(N,NSP)).LT.1.D-30 ) THEN
                     SP_CX = 0.D+0
                   ELSE
                     SP_CX = SP_C(N,NSP)
                   ENDIF
                   DVAR(N) = DVAR(N) + EQ_C(M,NEQ)*SP_CX
                 ENDIF
  220           CONTINUE
  222         CONTINUE
              WRITE(IPL,'(/,4A)') 'Exchange Nonaqueous-Liquid ', 
     &          SOLUT(NSL)(1:IDB),' Concentration, ',UNPLOT(IPNV)
              WRITE(IPL,FORM2) ( VAR*DVAR(N)*YN(N,NSL)/
     &          (SN(2,N)*PORD(2,N)+SMALL),N=1,NFX )
            ELSE
              WRITE(IPL,'(/,3A)') SOLUT(NSL)(1:IDB),
     &          ' Sorbed Concentration, ',UNPLOT(IPNV)
              WRITE(IPL,FORM2) (VAR*YG(N,NSL),N=1,NFX)
            ENDIF
          ELSEIF( IPNVX.EQ.22 ) THEN
            INDX = 4
            IUNS = -1
            CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
            IF( NSL.GT.NSOLU ) VAR = VAR*1.D-3
            WRITE(IPL,'(/,3A)') SOLUT(NSL)(1:IDB),
     &        ' Bubble-Gas Flux, ',UNPLOT(IPNV)
            WRITE(IPL,FORM2) (VAR*YN(N,NSL),N=1,NFX)
          ELSEIF( IPNVX.EQ.23 ) THEN
            WRITE(IPL,'(/,2A)') SOLUT(NSL)(1:IDB),' Solute Mass'
            VAR = 1.D+0
            IF( NSL.GT.NSOLU ) VAR = VAR*1.D-3
            IF( ISLC(13).EQ.1 ) THEN
              WRITE(IPL,FORM2) (VAR*(C(N,NSL)*VOL(N) +
     &          CNL(N,NSL)*SN(2,N)*VOL(N)),N=1,NFX)
            ELSE
              WRITE(IPL,FORM2) ((VAR*C(N,NSL)*VOL(N)),N=1,NFX)
            ENDIF
          ELSEIF( IPNVX.EQ.26 ) THEN
            INDX = 4
            IUNM = -3
            CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
            IF( NSL.GT.NSOLU )THEN
              NEQ = NSL-NSOLU
              VAR = VAR*1.D-3
              DO 232 N = 1,NFX
                DVAR(N) = 0.D+0
                DO 230 M = 1,IEQ_C(1,NEQ)
                 NSP = IEQ_C(M+1,NEQ)
                 IF( NSP.GT.NSPL .AND. NSP.LE.NSPL+NSPS ) THEN
                   IF( ABS(SP_C(N,NSP)).LT.1.D-30 ) THEN
                     SP_CX = 0.D+0
                   ELSE
                     SP_CX = SP_C(N,NSP)
                   ENDIF
                   DVAR(N) = DVAR(N) + EQ_C(M,NEQ)*SP_CX
                 ENDIF
  230           CONTINUE
  232         CONTINUE
              WRITE(IPL,'(/,4A)') 'Solid Volumetric ', 
     &          SOLUT(NSL)(1:IDB),' Concentration, ',UNPLOT(IPNV)
              WRITE(IPL,FORM2) (VAR*DVAR(N),N=1,NFX)
            ENDIF
          ELSEIF( IPNVX.EQ.27 ) THEN
            INDX = 4
            IUNM = -3
            CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
            IF( NSL.GT.NSOLU )THEN
              NEQ = NSL-NSOLU
              VAR = VAR*1.D-3
              DO 242 N = 1,NFX
                DVAR(N) = 0.D+0
                DO 240 M = 1,IEQ_C(1,NEQ)
                 NSP = IEQ_C(M+1,NEQ)
                 IF( NSP.GT.NSPL .AND. NSP.LE.NSPL+NSPS ) THEN
                   IF( ABS(SP_C(N,NSP)).LT.1.D-30 ) THEN
                     SP_CX = 0.D+0
                   ELSE
                     SP_CX = SP_C(N,NSP)
                   ENDIF
                   DVAR(N) = DVAR(N) + EQ_C(M,NEQ)*SP_CX
                 ENDIF
  240           CONTINUE
  242         CONTINUE
              WRITE(IPL,'(/,4A)') 'Solid Aqueous ', 
     &          SOLUT(NSL)(1:IDB),' Concentration, ',UNPLOT(IPNV)
              WRITE(IPL,FORM2) ( VAR*DVAR(N)*YL(N,NSL)/
     &          (SL(2,N)*PORD(2,N)+SMALL),N=1,NFX )
            ENDIF
          ELSEIF( IPNVX.EQ.28 ) THEN
            INDX = 4
            IUNM = -3
            CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
            IF( NSL.GT.NSOLU )THEN
              NEQ = NSL-NSOLU
              VAR = VAR*1.D-3
              DO 252 N = 1,NFX
                DVAR(N) = 0.D+0
                DO 250 M = 1,IEQ_C(1,NEQ)
                 NSP = IEQ_C(M+1,NEQ)
                 IF( NSP.GT.NSPL .AND. NSP.LE.NSPL+NSPS ) THEN
                   IF( ABS(SP_C(N,NSP)).LT.1.D-30 ) THEN
                     SP_CX = 0.D+0
                   ELSE
                     SP_CX = SP_C(N,NSP)
                   ENDIF
                   DVAR(N) = DVAR(N) + EQ_C(M,NEQ)*SP_CX
                 ENDIF
  250           CONTINUE
  252         CONTINUE
              WRITE(IPL,'(/,4A)') 'Solid Gas ', 
     &          SOLUT(NSL)(1:IDB),' Concentration, ',UNPLOT(IPNV)
              WRITE(IPL,FORM2) ( VAR*DVAR(N)*YG(N,NSL)/
     &          (SG(2,N)*PORD(2,N)+SMALL),N=1,NFX )
            ENDIF
          ELSEIF( IPNVX.EQ.29 ) THEN
            INDX = 4
            IUNM = -3
            CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
            IF( NSL.GT.NSOLU )THEN
              NEQ = NSL-NSOLU
              VAR = VAR*1.D-3
              DO 262 N = 1,NFX
                DVAR(N) = 0.D+0
                DO 260 M = 1,IEQ_C(1,NEQ)
                 NSP = IEQ_C(M+1,NEQ)
                 IF( NSP.GT.NSPL .AND. NSP.LE.NSPL+NSPS ) THEN
                   IF( ABS(SP_C(N,NSP)).LT.1.D-30 ) THEN
                     SP_CX = 0.D+0
                   ELSE
                     SP_CX = SP_C(N,NSP)
                   ENDIF
                   DVAR(N) = DVAR(N) + EQ_C(M,NEQ)*SP_CX
                 ENDIF
  260           CONTINUE
  262         CONTINUE
              WRITE(IPL,'(/,4A)') 'Solid Nonaqueous-Liquid ', 
     &          SOLUT(NSL)(1:IDB),' Concentration, ',UNPLOT(IPNV)
              WRITE(IPL,FORM2) ( VAR*DVAR(N)*YN(N,NSL)/
     &          (SN(2,N)*PORD(2,N)+SMALL),N=1,NFX )
            ENDIF
!
!---      Mass concentration  ---
!
          ELSEIF( IPNVX.EQ.30 ) THEN
            INDX = 4
            IUNKG = -1
            CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
            IF( NSL.GT.NSOLU ) THEN
              NEQ = NSL-NSOLU
              VAR = VAR*1.D-3
              DO 272 N = 1,NFLD
                DVAR(N) = 0.D+0
                DO 270 M = 1,IEQ_C(1,NEQ)
                  NSP = IEQ_C(M+1,NEQ)
                  IF( NSP.LE.NSPL ) THEN
                    IF( ABS(SP_C(N,NSP)).LT.1.D-30 ) THEN
                      SP_CX = 0.D+0
                    ELSE
                      SP_CX = SP_C(N,NSP)
                    ENDIF
                    DVAR(N) = DVAR(N) + EQ_C(M,NEQ)*SP_CX
                  ENDIF
  270           CONTINUE
  272         CONTINUE
              WRITE(IPL,'(/,4A)') 'Mass ',
     &          SOLUT(NSL)(1:IDB), ' Concentration, ',UNPLOT(IPNV)
              WRITE(IPL,FORM2) ( VAR*DVAR(N)/
     &          ((1.D+0-PORD(2,N))*RHOS(IZ(N))+SMALL),N=1,NFLD )
            ELSE
              WRITE(IPL,'(/,4A)') 'Mass ',
     &          SOLUT(NSL)(1:IDB), ' Concentration, ',UNPLOT(IPNV)
              WRITE(IPL,FORM2) (VAR*C(N,NSL)/
     &          ((1.D+0-PORD(2,N))*RHOS(IZ(N))+SMALL),N=1,NFLD )
            ENDIF
          ENDIF
        ENDIF
!
!---    Reactive species plot output ---
!
        INDX = 400+(NSOLU*33)+((NEQC+NEQK)*33)
        IF( IPNV.GT.INDX .AND. 
     &    IPNV.LE.(INDX+NSPR*33) ) THEN
          NSP = ((IPNV-INDX)/33) + 1
          IPNVX = MOD((IPNV-INDX),33)
          IF( NSP.GT.NSPL+NSPS+NSPE ) THEN
            IDB = INDEX( SPNMG(NSP-NSPL-NSPS-NSPE)(1:),'  ') - 1
            SPNMX = SPNMG(NSP-NSPL-NSPS-NSPE)
          ELSEIF( NSP.GT.NSPL+NSPS ) THEN
            IDB = INDEX( SPNME(NSP-NSPL-NSPS)(1:),'  ') - 1
            SPNMX = SPNME(NSP-NSPL-NSPS)
          ELSEIF( NSP.GT.NSPL ) THEN
            IDB = INDEX( SPNMS(NSP-NSPL)(1:),'  ') - 1
            SPNMX = SPNMS(NSP-NSPL)
          ELSE
            IDB = INDEX( SPNML(NSP)(1:),'  ') - 1
            SPNMX = SPNML(NSP)
          ENDIF
          IF( IPNVX.EQ.1 ) THEN
            INDX = 4
            IUNMOL = 1
            IUNM = -3
            CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
            VAR = VAR*1.D-3
            WRITE(IPL,'(/,4A)') 'Volumetric ',SPNMX(1:IDB),
     &        ' Concentration, ',UNPLOT(IPNV)
            IF( NSP.GT.NSPL .AND. NSP.LE.NSPL+NSPS+NSPE ) THEN
              NSP_M = NSP-NSPL
              IF( ISP_MN(NSP).EQ.1 ) THEN
                WRITE(IPL,FORM2) ( VAR*(SP_C(N,NSP)+
     &            SP_CMN(N,NSP_M)),N=1,NFX )
              ELSE
                WRITE(IPL,FORM2) ( VAR*SP_C(N,NSP),N=1,NFX )
              ENDIF
            ELSE
              WRITE(IPL,FORM2) ( VAR*SP_C(N,NSP),N=1,NFX )
            ENDIF
          ELSEIF( IPNVX.EQ.2 ) THEN
            INDX = 4
            IUNMOL = 1
            IUNM = -3
            CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
            VAR = VAR*1.D-3
            WRITE(IPL,'(/,4A)') 'Aqueous ',SPNMX(1:IDB),
     &        ' Concentration, ',UNPLOT(IPNV)
            IF( NSP.GT.NSPL .AND. NSP.LE.NSPL+NSPS ) THEN
              NSP_M = NSP-NSPL
              IF( ISP_MN(NSP).EQ.1 ) THEN
                WRITE(IPL,FORM2) ( VAR*(SP_C(N,NSP)+SP_CMN(N,NSP_M))/
     &            (SL(2,N)*PORD(2,N)+SMALL),N=1,NFX )
              ELSE
                WRITE(IPL,FORM2) ( VAR*SP_C(N,NSP)/
     &            (SL(2,N)*PORD(2,N)+SMALL),N=1,NFX )
              ENDIF
            ELSE
              WRITE(IPL,FORM2) ( VAR*SP_C(N,NSP)/
     &          (SL(2,N)*PORD(2,N)+SMALL),N=1,NFX )
            ENDIF
          ELSEIF( IPNVX.EQ.3 ) THEN
            INDX = 4
            IUNMOL = 1
            IUNM = -3
            CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
            VAR = VAR*1.D-3
            WRITE(IPL,'(/,4A)') 'Gas ',SPNMX(1:IDB),
     &        ' Concentration,',UNPLOT(IPNV)
            WRITE(IPL,FORM2) (VAR*SP_C(N,NSP)/
     &        (SG(2,N)*PORD(2,N)+SMALL),N=1,NFX)
          ELSEIF( IPNVX.EQ.4 ) THEN
            INDX = 4
            IUNMOL = 1
            IUNM = -3
            CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
            VAR = VAR*1.D-3
            WRITE(IPL,'(/,4A)') 'NAPL ',SPNMX(1:IDB),
     &        ' Concentration, ',UNPLOT(IPNV)
            WRITE(IPL,FORM2) (VAR*SP_C(N,NSP)/
     &        (SN(2,N)*PORD(2,N)+SMALL),N=1,NFX)
          ELSEIF( IPNVX.EQ.11 ) THEN
            INDX = 4
            IUNMOL = 1
            CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
            VAR = VAR*1.D-3
            WRITE(IPL,'(/,4A)') 'Source Integral of ',SPNMX(1:IDB),
     &        ', ',UNPLOT(IPNV)
            WRITE(IPL,FORM2) (VAR*SRCIC(N,NSL),N=1,NFX)
          ELSEIF( IPNVX.EQ.24 ) THEN
            INDX = 4
            IUNM = 2
            CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
            WRITE(IPL,'(/,4A)') SPNMX(1:IDB),' Area, ',UNPLOT(IPNV)
            IF( NSP.GT.NSPL .AND. NSP.LE.NSPL+NSPS+NSPE ) THEN
              NSP_M = NSP-NSPL
              WRITE(IPL,FORM2) ( VAR*(SP_AREA(N,NSP_M)),N=1,NFX )
            ENDIF
          ELSEIF( IPNVX.EQ.25 ) THEN
            INDX = 4
            IUNMOL = 1
            IUNS = -1
            CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
            VAR = VAR*1.D-3
            WRITE(IPL,'(/,4A)') SPNMX(1:IDB),' Rate, ',UNPLOT(IPNV)
            IF( NSP.GT.NSPL .AND. NSP.LE.NSPL+NSPS+NSPE ) THEN
              NSP_M = NSP-NSPL
              WRITE(IPL,FORM2) ( VAR*(SP_RATE(N,NSP_M)),N=1,NFX )
            ENDIF
          ELSEIF( IPNVX.EQ.26 ) THEN
            WRITE(IPL,'(/,4A)') SPNMX(1:IDB),' Volume Fraction, ',
     &        UNPLOT(IPNV)
            IF( NSP.GT.NSPL .AND. NSP.LE.NSPL+NSPS+NSPE ) THEN
              NSP_M = NSP-NSPL
              WRITE(IPL,FORM2) ( VAR*(RS_S(3,NSP_M,N)),N=1,NFX )
            ENDIF
          ELSEIF( IPNVX.EQ.27 ) THEN
            WRITE(IPL,'(/,A)') ' pH '
            WRITE(IPL,FORM2) ( C_PH(N),N=1,NFX )
          ENDIF
        ENDIF
  500 CONTINUE
!
!---  X-Dir. Velocity or Flux Variables  ---
!
      DO 700 NV = 1,NVPLOT
        VAR = 1.D+0
        IPNV = IPLOT(NV)
        IF( IPNV.EQ.114 ) THEN
          INDX = 4
          IUNKG = 1
          IUNM = -2
          IUNS = -1
          CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
          WRITE(IPL,'(/,2A)') 'X-Dir. Surfactant Flux, ',
     &      UNPLOT(IPNV)
          NX = (IFLD+1)*JFLD*KFLD
          WRITE(IPL,FORM2) (VAR*US(1,N),N=1,NX)
        ELSEIF( IPNV.EQ.120 ) THEN
          INDX = 4
          IUNKG = 1
          IUNM = -2
          IUNS = -1
          CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
          WRITE(IPL,'(/,2A)') 'X-Dir. Dissolved-Oil Flux, ',
     &      UNPLOT(IPNV)
          NX = (IFLD+1)*JFLD*KFLD
          WRITE(IPL,FORM2) (VAR*ULO(1,N),N=1,NX)
        ENDIF
!
!---    Solute plot output ---
!
        INDX = 400+(NSOLU*33)+((NEQC+NEQK)*33)
        IF( IPNV.GT.400 .AND. IPNV.LE.INDX ) THEN
          NSL = ((IPNV-400)/33) + 1
          IPNVX = MOD((IPNV-400),33)
          IDB = INDEX( SOLUT(NSL)(1:),'  ') - 1
          IF( IPNVX.EQ.8 ) THEN
            INDX = 4
            IUNM = -2
            IUNS = -1
            CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
            WRITE(IPL,'(/,4A)') 'X-Dir. ',SOLUT(NSL)(1:IDB),
     &        ' Flux, ',UNPLOT(IPNV)
            NX = (IFLD+1)*JFLD*KFLD
            WRITE(IPL,FORM2) (VAR*UC(N,NSL),N=1,NX)
          ELSEIF( IPNVX.EQ.16 ) THEN
            INDX = 4
            IUNM = -2
            IUNS = -1
            CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
            WRITE(IPL,'(/,4A)') 'X-Dir. Aqueous ',SOLUT(NSL)(1:IDB),
     &        ' Flux, ',UNPLOT(IPNV)
            NX = (IFLD+1)*JFLD*KFLD
            WRITE(IPL,FORM2) (VAR*UC(N,NSL),N=1,NX)
          ELSEIF( IPNVX.EQ.19 ) THEN
            INDX = 4
            IUNM = 2
            IUNS = -1
            CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
            WRITE(IPL,'(/,4A)') 'X-Dir. NAPL ',SOLUT(NSL)(1:IDB),
     &        ' Flux, ',UNPLOT(IPNV)
            NX = (IFLD+1)*JFLD*KFLD
            WRITE(IPL,FORM2) (VAR*UCN(N,NSL),N=1,NX)
          ENDIF
        ENDIF
  700 CONTINUE
!
!---  Y-Dir. Velocity or Flux Variables  ---
!
      DO 800 NV = 1,NVPLOT
        VAR = 1.D+0
        IPNV = IPLOT(NV)
        IF( IPNV.EQ.115 ) THEN
          INDX = 4
          IUNKG = 1
          IUNM = -2
          IUNS = -1
          CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
          WRITE(IPL,'(/,2A)') 'Y-Dir. Surfactant Flux, ',
     &      UNPLOT(IPNV)
          NY = IFLD*(JFLD+1)*KFLD
          WRITE(IPL,FORM2) (VAR*VS(1,N),N=1,NY)
        ELSEIF( IPNV.EQ.121 ) THEN
          INDX = 4
          IUNKG = 1
          IUNM = -2
          IUNS = -1
          CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
          WRITE(IPL,'(/,2A)') 'Y-Dir. Dissolved-Oil Flux, ',
     &      UNPLOT(IPNV)
          NY = IFLD*(JFLD+1)*KFLD
          WRITE(IPL,FORM2) (VAR*VLO(1,N),N=1,NY)
        ENDIF
!
!---    Solute plot output ---
!
        INDX = 400+(NSOLU*33)+((NEQC+NEQK)*33)
        IF( IPNV.GT.400 .AND. IPNV.LE.INDX ) THEN
          NSL = ((IPNV-400)/33) + 1
          IPNVX = MOD((IPNV-400),33)
          IDB = INDEX( SOLUT(NSL)(1:),'  ') - 1
          IF( IPNVX.EQ.9 ) THEN
            INDX = 4
            IUNM = -2
            IUNS = -1
            CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
            WRITE(IPL,'(/,4A)') 'Y-Dir. ',SOLUT(NSL)(1:IDB),' Flux, ',
     &        UNPLOT(IPNV)
            NY = IFLD*(JFLD+1)*KFLD
            WRITE(IPL,FORM2) (VAR*VC(N,NSL),N=1,NY)
          ELSEIF( IPNVX.EQ.17 ) THEN
            INDX = 4
            IUNM = -2
            IUNS = -1
            CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
            WRITE(IPL,'(/,4A)') 'Y-Dir. Aqueous ',SOLUT(NSL)(1:IDB),
     &        ' Flux, ',UNPLOT(IPNV)
            NY = IFLD*(JFLD+1)*KFLD
            WRITE(IPL,FORM2) (VAR*VC(N,NSL),N=1,NY)
          ELSEIF( IPNVX.EQ.20 ) THEN
            INDX = 4
            IUNM = -2
            IUNS = -1
            CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
            WRITE(IPL,'(/,4A)') 'Y-Dir. NAPL ',SOLUT(NSL)(1:IDB),
     &        ' Flux, ',UNPLOT(IPNV)
            NY = IFLD*(JFLD+1)*KFLD
            WRITE(IPL,FORM2) (VAR*VCN(N,NSL),N=1,NY)
          ENDIF
        ENDIF
  800 CONTINUE
!
!---  Z-Dir. Velocity or Flux Variables  ---
!
      DO 900 NV = 1,NVPLOT
        VAR = 1.D+0
        IPNV = IPLOT(NV)
        IF( IPNV.EQ.116 ) THEN
          INDX = 4
          IUNKG = 1
          IUNM = -2
          IUNS = -1
          CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
          WRITE(IPL,'(/,2A)') 'Z-Dir. Surfactant Flux, ',
     &      UNPLOT(IPNV)
          NZ = IFLD*JFLD*(KFLD+1)
          WRITE(IPL,FORM2) (VAR*WS(1,N),N=1,NZ)
        ELSEIF( IPNV.EQ.122 ) THEN
          INDX = 4
          IUNKG = 1
          IUNM = -2
          IUNS = -1
          CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
          WRITE(IPL,'(/,2A)') 'Z-Dir. Dissolved-Oil Flux, ',
     &      UNPLOT(IPNV)
          NZ = IFLD*JFLD*(KFLD+1)
          WRITE(IPL,FORM2) (VAR*WLO(1,N),N=1,NZ)
        ENDIF
!
!---    Solute plot output ---
!
        INDX = 400+(NSOLU*33)+((NEQC+NEQK)*33)
        IF( IPNV.GT.400 .AND. IPNV.LE.INDX ) THEN
          NSL = ((IPNV-400)/33) + 1
          IPNVX = MOD((IPNV-400),33)
          IDB = INDEX( SOLUT(NSL)(1:),'  ') - 1
          IF( IPNVX.EQ.10 ) THEN
            INDX = 4
            IUNM = -2
            IUNS = -1
            CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
            WRITE(IPL,'(/,4A)') 'Z-Dir. ',SOLUT(NSL)(1:IDB),' Flux, ',
     &        UNPLOT(IPNV)
            NZ = IFLD*JFLD*(KFLD+1)
            WRITE(IPL,FORM2) (VAR*WC(N,NSL),N=1,NZ)
          ELSEIF( IPNVX.EQ.18 ) THEN
            INDX = 4
            IUNM = -2
            IUNS = -1
            CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
            WRITE(IPL,'(/,4A)') 'Z-Dir. Aqueous ',SOLUT(NSL)(1:IDB),
     &        ' Flux, ',UNPLOT(IPNV)
            NZ = IFLD*JFLD*(KFLD+1)
            WRITE(IPL,FORM2) (VAR*WC(N,NSL),N=1,NZ)
          ELSEIF( IPNVX.EQ.21 ) THEN
            INDX = 4
            IUNM = -2
            IUNS = -1
            CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
            WRITE(IPL,'(/,4A)') 'Z-Dir. NAPL ',SOLUT(NSL)(1:IDB),
     &        ' Flux, ',UNPLOT(IPNV)
            NZ = IFLD*JFLD*(KFLD+1)
            WRITE(IPL,FORM2) (VAR*WCN(N,NSL),N=1,NZ)
          ENDIF
        ENDIF
  900 CONTINUE
!
!---  Close plot file  ---
!
      CLOSE( UNIT=IPL )
!
!---  Deallocate memory  ---
!
      DEALLOCATE( DVAR,STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Deallocation Error: DVAR'
        CALL WRMSGS( INDX )
      ENDIF
      DEALLOCATE( IVAR,STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Deallocation Error: IVAR'
        CALL WRMSGS( INDX )
      ENDIF
      ISUB_LOG = ISUB_LOG-1
!
!---  End of WRPLOT group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE WRPL_1( FORM,IPNV,IPNVGC,NFX )
!
!-------------------------Disclaimer-----------------------------------!
!
!     This material was prepared as an account of work sponsored by
!     an agency of the United States Government. Neither the
!     United States Government nor the United States Department of
!     Energy, nor Battelle, nor any of their employees, makes any
!     warranty, express or implied, or assumes any legal liability or
!     responsibility for the accuracy, completeness, or usefulness
!     of any information, apparatus, product, software or process
!     disclosed, or represents that its use would not infringe
!     privately owned rights.
!
!----------------------Acknowledgement---------------------------------!
!
!     This software and its documentation were produced with Government
!     support under Contract Number DE-AC06-76RLO-1830 awarded by the
!     United Department of Energy. The Government retains a paid-up
!     non-exclusive, irrevocable worldwide license to reproduce,
!     prepare derivative works, perform publicly and display publicly
!     by or for the Government, including the right to distribute to
!     other Government contractors.
!
!---------------------Copyright Notices--------------------------------!
!
!            Copyright Battelle Memorial Institute, 1996
!                    All Rights Reserved.
!
!----------------------Description-------------------------------------!
!
!     Write plot files for the following variables:
!
!     1                Aqueous pressure
!     2                Gas pressure
!     3    WAE         Surface vapor pressure
!     3                NAPL pressure
!     3    HYD-KE      Liquid-CO2 pressure
!     4                Temperature
!     5                Phase condition
!     6                Aqueous gauge pressure
!     7                Gas gauge pressure
!     8                NAPL gauge pressure
!     8    HYD-KE      Liquid-CO2 pressure
!     9                Apparent aqueous saturation
!     10               Apparent total-liquid saturation
!     11               Aqueous saturation
!     12               Gas saturation
!     13   WAE         Surface temperature
!     13               NAPL saturation
!     13   HYD-KE      Liquid-CO2 saturation
!     14               Total-liquid saturation
!     15               Aqueous moisture content
!     16   CO2         Aqueous CO2 mole fraction
!     16   CO2e        Aqueous CO2 mole fraction
!     16   CO2ae       Aqueous CO2 mole fraction
!     16               NAPL moisture content
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 1 January 2003.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE OUTPU
      USE HYST
      USE GRID
      USE FILES
      USE FDVP
      USE CONST
!

!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Parameter Statements----------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*16 FORM
      REAL*8, DIMENSION(:), ALLOCATABLE ::  DVAR
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/WRPL_1'
      IF( INDEX(SVN_ID(277)(1:1),'$').EQ.0 ) SVN_ID(277) =
     & '$Id: wrplot.F 1080 2017-03-14 16:22:02Z d3c002 $' 
!
!---  Dynamic memory allocation  ---
!
      ALLOCATE( DVAR(1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DVAR'
        CALL WRMSGS( INDX )
      ENDIF
      VAR = 1.D+0
      IPNVGCX = IPNVGC
      IF( IPNV.EQ.1 ) THEN
        INDX = 4
        IUNM = -1
        IUNKG = 1
        IUNS = -2
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Aqueous Pressure, ',UNPLOT(IPNV)
        WRITE(IPL,FORM) (VAR*(PL(2,N)+PATM),N=1,NFX)
      ELSEIF( IPNV.EQ.2 ) THEN
        INDX = 4
        IUNM = -1
        IUNKG = 1
        IUNS = -2
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Gas Pressure, ',UNPLOT(IPNV)
        WRITE(IPL,FORM) (VAR*(PG(2,N)+PATM),N=1,NFX)
      ELSEIF( IPNV.EQ.3 ) THEN
        INDX = 4
        IUNM = -1
        IUNKG = 1
        IUNS = -2
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        IF( IOM.EQ.1 ) THEN
          WRITE(IPL,'(/,2A)') 'Matrix Pressure, ',
     &      UNPLOT(IPNV)
        ELSEIF( IOM.EQ.3 ) THEN
          WRITE(IPL,'(/,2A)') 'Surface Vapor Pressure, ',
     &      UNPLOT(IPNV)
        ELSEIF( IOM.EQ.38 ) THEN
          WRITE(IPL,'(/,2A)') 'Liquid CO2 Pressure, ',
     &      UNPLOT(IPNV)
        ELSEIF( IOM.EQ.39 ) THEN
          WRITE(IPL,'(/,2A)') 'Nonaqueous Liquid Pressure, ',
     &      UNPLOT(IPNV)
        ELSE
          WRITE(IPL,'(/,2A)') 'NAPL Pressure, ',UNPLOT(IPNV)
        ENDIF
        WRITE(IPL,FORM) (VAR*(PN(2,N)+PATM),N=1,NFX)
      ELSEIF( IPNV.EQ.4 ) THEN
        WRITE(IPL,'(/,2A)') 'Temperature, ',UNPLOT(IPNV)
        IF( UNPLOT(IPNV).EQ.'c' ) THEN
          WRITE(IPL,FORM) (T(2,N),N=1,NFX)
        ELSEIF( UNPLOT(IPNV).EQ.'k' ) THEN
          WRITE(IPL,FORM) ((T(2,N)+273.15D+0),N=1,NFX)
        ELSEIF( UNPLOT(IPNV).EQ.'f' ) THEN
          WRITE(IPL,FORM) ((T(2,N)*1.8D+0+3.2D+1),N=1,NFX)
        ELSEIF( UNPLOT(IPNV).EQ.'r' ) THEN
          WRITE(IPL,FORM) ((T(2,N)*1.8D+0+4.92D+2),N=1,NFX)
        ENDIF
      ELSEIF( IPNV.EQ.5 ) THEN
        WRITE(IPL,'(/,A)') 'Phase Condition, '
        DO 5 N = 1,NFX
          DVAR(N) = REAL(NPHAZ(2,N))
          IF( IXP(N).EQ.0 ) DVAR(N) = 0.D+0
    5   CONTINUE
        WRITE(IPL,FORM) (DVAR(N),N=1,NFX)
      ELSEIF( IPNV.EQ.6 ) THEN
        INDX = 4
        IUNM = -1
        IUNKG = 1
        IUNS = -2
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)')'Aqueous Gauge Pressure, ',UNPLOT(IPNV)
        WRITE(IPL,FORM) (VAR*PL(2,N),N=1,NFX)
      ELSEIF( IPNV.EQ.7 ) THEN
        INDX = 4
        IUNM = -1
        IUNKG = 1
        IUNS = -2
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Gas Gauge Pressure, ',UNPLOT(IPNV)
        WRITE(IPL,FORM) (VAR*PG(2,N),N=1,NFX)
      ELSEIF( IPNV.EQ.8 ) THEN
        INDX = 4
        IUNM = -1
        IUNKG = 1
        IUNS = -2
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        IF( IOM.GE.38 ) THEN
          WRITE(IPL,'(/,2A)') 'NAPL Gauge Pressure, ',UNPLOT(IPNV)
        ELSEIF( IOM.GE.39 ) THEN
          WRITE(IPL,'(/,2A)') 'Nonaqueous Liquid Gauge Pressure, ',
     &      UNPLOT(IPNV)
        ELSE
          WRITE(IPL,'(/,2A)') 'Liquid-CO2 Gauge Pressure, ',UNPLOT(IPNV)
        ENDIF
        WRITE(IPL,FORM) (VAR*PN(2,N),N=1,NFX)
      ELSEIF( IPNV.EQ.9 ) THEN
        WRITE(IPL,'(/,A)') 'Apparent Aqueous Saturation'
        WRITE(IPL,FORM) (ASL(N),N=1,NFX)
      ELSEIF( IPNV.EQ.10 ) THEN
        WRITE(IPL,'(/,A)') 'Apparent Total Liquid Saturation'
        WRITE(IPL,FORM) (AST(N),N=1,NFX)
      ELSEIF( IPNV.EQ.11 ) THEN
        WRITE(IPL,'(/,A)') 'Aqueous Saturation'
        WRITE(IPL,FORM) (SL(2,N),N=1,NFX)
      ELSEIF( IPNV.EQ.12 ) THEN
        WRITE(IPL,'(/,A)') 'Gas Saturation'
        WRITE(IPL,FORM) (SG(2,N),N=1,NFX)
      ELSEIF( IPNV.EQ.13 ) THEN
        IF( IOM.GE.35 .AND. IOM.LE.38 ) THEN
          WRITE(IPL,'(/,A)') 'Liquid-CO2 Saturation'
          WRITE(IPL,FORM) (SN(2,N),N=1,NFX)
        ELSEIF( IOM.EQ.39 .OR. IOM.EQ.43 ) THEN
          WRITE(IPL,'(/,A)') 'Nonaqueous Liquid Saturation'
          WRITE(IPL,FORM) (SN(2,N),N=1,NFX)
        ELSEIF( IOM.EQ.3 ) THEN
          WRITE(IPL,'(/,A)') 'Surface Temperature'
          IF( UNPLOT(IPNV).EQ.'c' ) THEN
            WRITE(IPL,FORM) (SN(2,N),N=1,NFX)
          ELSEIF( UNPLOT(IPNV).EQ.'k' ) THEN
            WRITE(IPL,FORM) ((SN(2,N)+273.15D+0),N=1,NFX)
          ELSEIF( UNPLOT(IPNV).EQ.'f' ) THEN
            WRITE(IPL,FORM) ((SN(2,N)*1.8D+0+3.2D+1),N=1,NFX)
          ELSEIF( UNPLOT(IPNV).EQ.'r' ) THEN
            WRITE(IPL,FORM) ((SN(2,N)*1.8D+0+4.92D+2),N=1,NFX)
          ENDIF
        ELSE
          WRITE(IPL,'(/,A)') 'NAPL Saturation'
          WRITE(IPL,FORM) (SN(2,N),N=1,NFX)
        ENDIF
      ELSEIF( IPNV.EQ.14 ) THEN
        WRITE(IPL,'(/,A)') 'Total Liquid Saturation'
        WRITE(IPL,FORM) ((SL(2,N)+SN(2,N)),N=1,NFX)
      ELSEIF( IPNV.EQ.15 ) THEN
        WRITE(IPL,'(/,A)') 'Aqueous Moisture Content'
        WRITE(IPL,FORM) ((SL(2,N)*PORD(2,N)),N=1,NFX)
      ELSEIF( IPNV.EQ.16 ) THEN
        IF( IOM.GE.32 .AND. IOM.LE.39 ) THEN
          WRITE(IPL,'(/,A)') 'Aqueous CO2 Mole Fraction'
          WRITE(IPL,FORM) (XMLA(2,N),N=1,NFX)
        ELSE
          WRITE(IPL,'(/,A)') 'NAPL Moisture Content'
          WRITE(IPL,FORM) ((SN(2,N)*PORD(2,N)),N=1,NFX)
        ENDIF
      ENDIF
!
!---  Deallocate memory  ---
!
      DEALLOCATE( DVAR,STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Deallocation Error: DVAR'
        CALL WRMSGS( INDX )
      ENDIF
      ISUB_LOG = ISUB_LOG-1
!
!---  End of WRPL_1 group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE WRPL_2( FORM,IPNV,IPNVGC,NFX )
!
!-------------------------Disclaimer-----------------------------------!
!
!     This material was prepared as an account of work sponsored by
!     an agency of the United States Government. Neither the
!     United States Government nor the United States Department of
!     Energy, nor Battelle, nor any of their employees, makes any
!     warranty, express or implied, or assumes any legal liability or
!     responsibility for the accuracy, completeness, or usefulness
!     of any information, apparatus, product, software or process
!     disclosed, or represents that its use would not infringe
!     privately owned rights.
!
!----------------------Acknowledgement---------------------------------!
!
!     This software and its documentation were produced with Government
!     support under Contract Number DE-AC06-76RLO-1830 awarded by the
!     United Department of Energy. The Government retains a paid-up
!     non-exclusive, irrevocable worldwide license to reproduce,
!     prepare derivative works, perform publicly and display publicly
!     by or for the Government, including the right to distribute to
!     other Government contractors.
!
!---------------------Copyright Notices--------------------------------!
!
!            Copyright Battelle Memorial Institute, 1996
!                    All Rights Reserved.
!
!----------------------Description-------------------------------------!
!
!     Write plot files for the following variables:
!
!     17   IOM == 32   Aqueous NaCl mole fraction
!     17   IOM == 33   Aqueous NaCl mole fraction
!     17   IOM == 34   Aqueous NaCl mole fraction
!     17               Total-liquid moisture content
!     18               Effective trapped-NAPL saturation
!     19               Effective trapped-gas saturation
!     20               Diffusive porosity
!     21               Gas water mass fraction
!     22               Gas air mass fraction
!     22   IOM == 30   Gas gas component mass fraction
!     22   IOM == 32   Gas CO2 mass fraction
!     22   IOM == 33   Gas CO2 mass fraction
!     22   IOM == 34   Gas CO2 mass fraction
!     22   IOM == 36   Gas CO2 mass fraction
!     22   IOM == 43   Gas petroleum mass fraction
!     23               Gas oil mass fraction
!     23   IOM == 36   Gas CH4 mass fraction
!     24               Aqueous water mass fraction
!     25               Aqueous air mass fraction
!     25   IOM = 43    Aqueous CO2 mass fraction
!     26               Aqueous oil mass fraction
!     27               Aqueous Hydraulic Head (Fresh Water)
!     28               Gas Hydraulic Head (Fresh Water)
!     29               NAPL Hydraulic Head (Fresh Water)
!     30               Rock/soil type
!     31               Aqueous relative permeability
!     32   IOM == 4    Residual NAPL saturation
!     32               Gas relative permeability
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 1 January 2003.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE OUTPU
      USE HYST
      USE GRID
      USE FILES
      USE FDVS
      USE FDVP
      USE FDVGC
      USE FDVG
      USE CONST
!

!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Parameter Statements----------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*16 FORM
      REAL*8, DIMENSION(:), ALLOCATABLE ::  DVAR
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/WRPL_2'
      IF( INDEX(SVN_ID(277)(1:1),'$').EQ.0 ) SVN_ID(277) =
     & '$Id: wrplot.F 1080 2017-03-14 16:22:02Z d3c002 $' 
!
!---  Dynamic memory allocation  ---
!
      ALLOCATE( DVAR(1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DVAR'
        CALL WRMSGS( INDX )
      ENDIF
      VAR = 1.D+0
      IF( IPNV.EQ.17 ) THEN
        IF( IOM.GE.32 .AND. IOM.LE.39 ) THEN
          WRITE(IPL,'(/,A)') 'Aqueous NaCl Mole Fraction'
          WRITE(IPL,FORM) (XMLS(2,N),N=1,NFX)
        ELSE
          WRITE(IPL,'(/,A)') 'Total Liquid Moisture Content'
          WRITE(IPL,FORM)
     &      (((SL(2,N)+SN(2,N))*PORD(2,N)),N=1,NFX)
        ENDIF
      ELSEIF( IPNV.EQ.18 ) THEN
        WRITE(IPL,'(/,A)') 'Effective Trapped NAPL Saturation'
        WRITE(IPL,FORM) (ASNT(N),N=1,NFX)
      ELSEIF( IPNV.EQ.19 ) THEN
        WRITE(IPL,'(/,A)') 'Effective Trapped Gas Saturation'
        WRITE(IPL,FORM) (ASGT(N),N=1,NFX)
      ELSEIF( IPNV.EQ.20 ) THEN
        WRITE(IPL,'(/,A)') 'Diffusive Porosity'
        WRITE(IPL,FORM) (PORD(2,N),N=1,NFX)
      ELSEIF( IPNV.EQ.21 ) THEN
        WRITE(IPL,'(/,A)') 'Gas Water Mass Fraction'
        WRITE(IPL,FORM) (XGW(2,N),N=1,NFX)
      ELSEIF( IPNV.EQ.22 ) THEN
        IF( IOM.EQ.30 .OR. IOM.EQ.40 .OR. IOM.EQ.43 ) THEN
          NCHX = INDEX(GCNM(IPNVGC)(1:),'  ')-1
          WRITE(IPL,'(/,A)') 'Gas ' // GCNM(IPNVGC)(1:NCHX)
     &      // ' Mass Fraction'
          WRITE(IPL,FORM) (XGC(IPNVGC,2,N),N=1,NFX)
        ELSEIF( IOM.GE.32 .AND. IOM.LE.39 ) THEN
          WRITE(IPL,'(/,A)') 'Gas CO2 Mass Fraction'
          WRITE(IPL,FORM) (XGA(2,N),N=1,NFX)
        ELSE
          WRITE(IPL,'(/,A)') 'Gas Air Mass Fraction'
          WRITE(IPL,FORM) (XGA(2,N),N=1,NFX)
        ENDIF
      ELSEIF( IPNV.EQ.23 ) THEN
        IF( IOM.EQ.43 ) THEN
          NCHX = INDEX(GCNM(IPNVGC)(1:),'  ')-1
          WRITE(IPL,'(/,A)') 'Nonaqueous Liquid ' // 
     &      GCNM(IPNVGC)(1:NCHX) // ' Mass Fraction'
          WRITE(IPL,FORM) (XNC(IPNVGC,2,N),N=1,NFX)
        ELSEIF( IOM.GE.36 .AND. IOM.LE.39 ) THEN
          WRITE(IPL,'(/,A)') 'Gas CH4 Mass Fraction'
          WRITE(IPL,FORM) (XGO(2,N),N=1,NFX)
        ELSE
          WRITE(IPL,'(/,A)') 'Gas Oil Mass Fraction'
          WRITE(IPL,FORM) (XGO(2,N),N=1,NFX)
        ENDIF
      ELSEIF( IPNV.EQ.24 ) THEN
        WRITE(IPL,'(/,A)') 'Aqueous Water Mass Fraction'
        WRITE(IPL,FORM) (XLW(2,N),N=1,NFX)
      ELSEIF( IPNV.EQ.25 ) THEN
        IF( IOM.EQ.30 .OR. IOM.EQ.40 ) THEN
          NCH = INDEX(GCNM(IPNVGC)(1:),'  ')-1
          WRITE(IPL,'(/,A)') 'Aqueous ' // GCNM(IPNVGC)(1:NCH)
     &      // ' Mass Fraction'
          WRITE(IPL,FORM) (XLC(IPNVGC,2,N),N=1,NFX)
        ELSEIF( IOM.GE.32 .AND. IOM.LE.39 .AND. IOM.LE.43 ) THEN
          WRITE(IPL,'(/,A)') 'Aqueous CO2 Mass Fraction'
          WRITE(IPL,FORM) (XLA(2,N),N=1,NFX)
        ELSE
          WRITE(IPL,'(/,A)') 'Aqueous Air Mass Fraction'
          WRITE(IPL,FORM) (XLA(2,N),N=1,NFX)
        ENDIF
      ELSEIF( IPNV.EQ.26 ) THEN
        IF( IOM.GE.36 .AND. IOM.LE.39 ) THEN
          WRITE(IPL,'(/,A)') 'Aqueous CH4 Mass Fraction'
          WRITE(IPL,FORM) (XLO(2,N),N=1,NFX)
        ELSE
          WRITE(IPL,'(/,A)') 'Aqueous Oil Mass Fraction'
          WRITE(IPL,FORM) (XLO(2,N),N=1,NFX)
        ENDIF
      ELSEIF( IPNV.EQ.27 ) THEN
        INDX = 4
        IUNM = 1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Aqueous Hydraulic Head ' //
     &    '(Fresh Water),',UNPLOT(IPNV)
        WRITE(IPL,FORM)
     &    (VAR*(PL(2,N)/RHORL/GRAV + ZP(N)),N=1,NFX)
      ELSEIF( IPNV.EQ.28 ) THEN
        INDX = 4
        IUNM = 1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Gas Hydraulic Head ' //
     &    '(Fresh Water),',UNPLOT(IPNV)
        WRITE(IPL,FORM)
     &    (VAR*(PG(2,N)/RHORL/GRAV + ZP(N)),N=1,NFX)
      ELSEIF( IPNV.EQ.29 ) THEN
        INDX = 4
        IUNM = 1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'NAPL Hydraulic Head ' //
     &    '(Fresh Water),',UNPLOT(IPNV)
        WRITE(IPL,FORM)
     &    (VAR*(PN(2,N)/RHORL/GRAV + ZP(N)),N=1,NFX)
      ELSEIF( IPNV.EQ.30 ) THEN
        WRITE(IPL,'(/,A)') 'Rock/Soil Type'
        DO 30 N = 1,NFX
          DVAR(N) = REAL(IZ(N))
          IF( IXP(N).EQ.0 ) DVAR(N) = 0.D+0
   30   CONTINUE
        WRITE(IPL,FORM) (DVAR(N),N=1,NFX)
      ELSEIF( IPNV.EQ.31 ) THEN
        WRITE(IPL,'(/,A)') 'Aqueous Relative Permeability'
        DO 31 N = 1,NFX
          DVAR(N) = (RKL(1,2,N)*RKL(2,2,N)*RKL(3,2,N))**THIRD
   31   CONTINUE
        WRITE(IPL,FORM) (DVAR(N),N=1,NFX)
      ELSEIF( IPNV.EQ.32 ) THEN
        WRITE(IPL,'(/,A)') 'Gas Relative Permeability'
        WRITE(IPL,FORM) (RKG(2,N),N=1,NFX)
      ENDIF
!
!---  Deallocate memory  ---
!
      DEALLOCATE( DVAR,STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Deallocation Error: DVAR'
        CALL WRMSGS( INDX )
      ENDIF
      ISUB_LOG = ISUB_LOG-1
!
!---  End of WRPL_2 group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE WRPL_3( FORM,IPNV,IPNVGC,NFX )
!
!-------------------------Disclaimer-----------------------------------!
!
!     This material was prepared as an account of work sponsored by
!     an agency of the United States Government. Neither the
!     United States Government nor the United States Department of
!     Energy, nor Battelle, nor any of their employees, makes any
!     warranty, express or implied, or assumes any legal liability or
!     responsibility for the accuracy, completeness, or usefulness
!     of any information, apparatus, product, software or process
!     disclosed, or represents that its use would not infringe
!     privately owned rights.
!
!----------------------Acknowledgement---------------------------------!
!
!     This software and its documentation were produced with Government
!     support under Contract Number DE-AC06-76RLO-1830 awarded by the
!     United Department of Energy. The Government retains a paid-up
!     non-exclusive, irrevocable worldwide license to reproduce,
!     prepare derivative works, perform publicly and display publicly
!     by or for the Government, including the right to distribute to
!     other Government contractors.
!
!---------------------Copyright Notices--------------------------------!
!
!            Copyright Battelle Memorial Institute, 1996
!                    All Rights Reserved.
!
!----------------------Description-------------------------------------!
!
!     Write plot files for the following variables:
!
!     33               NAPL relative permeability
!     34               Aqueous density
!     35               Gas density
!     36               NAPL density
!     36    HYD-KE     Liquid-CO2 density
!     37               Total water mass
!     38               Total air mass
!     38    IOM=4      Mobile-NAPL saturation
!     38    IOM=30     Total gas component mass
!     38    IOM=32     Total CO2 mass
!     38    IOM=33     Total CO2 mass
!     38    IOM=34     Total CO2 mass
!     38    IOM=36     Total CO2 mass
!     38    IOM=40     Total gas component mass
!     39               Total oil mass
!     39    IOM=36     Total CH4 mass
!     40               Water mass source integral
!     41               Air mass source integral
!     41    IOM=32     CO2 mass source integral
!     41    IOM=33     CO2 mass source integral
!     41    IOM=34     CO2 mass source integral
!     41    IOM=36     CH4 mass source integral
!     42               Oil mass source integral
!     42    IOM=36     CH4 mass source integral
!     43               Thermal energy source integral
!     44               X-Dir. thermal conductivity
!     45               Y-Dir. thermal conductivity
!     46               Z-Dir. thermal conductivity
!     47               Salt volumetric concentration
!     48               Salt aqueous concentration
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 1 January 2003.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOURC
      USE SOLTN
      USE PORMED
      USE OUTPU
      USE GRID
      USE FILES
      USE FDVT
      USE FDVS
      USE FDVP
      USE FDVN
      USE FDVI
      USE FDVH
      USE FDVGC
      USE FDVA
      USE CONST
!

!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Parameter Statements----------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*16 FORM
      REAL*8, DIMENSION(:), ALLOCATABLE ::  DVAR
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/WRPL_3'
      IF( INDEX(SVN_ID(277)(1:1),'$').EQ.0 ) SVN_ID(277) =
     & '$Id: wrplot.F 1080 2017-03-14 16:22:02Z d3c002 $' 
!
!---  Dynamic memory allocation  ---
!
      ALLOCATE( DVAR(1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DVAR'
        CALL WRMSGS( INDX )
      ENDIF
      VAR = 1.D+0
      IF( IPNV.EQ.33 ) THEN
        IF( IOM.EQ.38 ) THEN
          WRITE(IPL,'(/,A)') 'Liquid-CO2 Relative Permeability'
        ELSEIF( IOM.EQ.39 ) THEN
          WRITE(IPL,'(/,A)') 'Nonaqueous Liquid Relative Permeability'
        ELSE
          WRITE(IPL,'(/,A)') 'NAPL Relative Permeability'
        ENDIF
        WRITE(IPL,FORM) (RKN(2,N),N=1,NFX)
      ELSEIF( IPNV.EQ.34 ) THEN
        INDX = 4
        IUNM = -3
        IUNKG = 1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)')
     &    'Aqueous Density, ',UNPLOT(IPNV)
        WRITE(IPL,FORM) (VAR*RHOL(2,N),N=1,NFX)
      ELSEIF( IPNV.EQ.35 ) THEN
        INDX = 4
        IUNM = -3
        IUNKG = 1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)')
     &    'Gas Density, ',UNPLOT(IPNV)
        WRITE(IPL,FORM) (VAR*RHOG(2,N),N=1,NFX)
      ELSEIF( IPNV.EQ.36 ) THEN
        INDX = 4
        IUNM = -3
        IUNKG = 1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        IF( IOM.EQ.37 .OR. IOM.EQ.38 ) THEN
          WRITE(IPL,'(/,2A)')
     &      'Liquid-CO2 Density, ',UNPLOT(IPNV)
        ELSEIF( IOM.EQ.39 ) THEN
          WRITE(IPL,'(/,2A)')
     &      'Nonaqueous Liquid Density, ',UNPLOT(IPNV)
        ELSE
          WRITE(IPL,'(/,2A)')
     &      'NAPL Density, ',UNPLOT(IPNV)
        ENDIF
        WRITE(IPL,FORM) (VAR*RHON(2,N),N=1,NFX)
      ELSEIF( IPNV.EQ.37 ) THEN
        INDX = 4
        IUNKG = 1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        DO 37 N = 1,NFX
          DVAR(N) = 0.D+0
!
!---      Aqueous water  ---
!
          IF( IAQU.EQ.1 ) DVAR(N) = DVAR(N) +
     &      VAR*PORD(2,N)*VOL(N)*XLW(2,N)*SL(2,N)*RHOL(2,N)
!
!---      Gas water  ---
!
          IF( IGAS.EQ.1 ) DVAR(N) = DVAR(N) +
     &      VAR*PORD(2,N)*VOL(N)*XGW(2,N)*SG(2,N)*RHOG(2,N)
!
!---      NAPL water  ---
!
          IF( INAPL.EQ.1 ) DVAR(N) = DVAR(N) +
     &      VAR*PORD(2,N)*VOL(N)*XNW(2,N)*SN(2,N)*RHON(2,N)
!
!---      Ice water  ---
!
          IF( LFDI.EQ.LFD ) THEN
            DVAR(N) = DVAR(N) +
     &        VAR*PORD(2,N)*VOL(N)*SI(2,N)*RHOI(2,N)
          ENDIF
!
!---      Hydrate water  ---
!
          IF( LFDH.EQ.LFD ) THEN
            DVAR(N) = DVAR(N) +
     &        VAR*PORD(2,N)*VOL(N)*XHW(2,N)*SH(2,N)*RHOH(2,N)
          ENDIF
   37   CONTINUE
        WRITE(IPL,'(/,2A)') 'Total Water Mass, ',UNPLOT(IPNV)
        WRITE(IPL,FORM) (DVAR(N),N=1,NFX)
      ELSEIF( IPNV.EQ.38 ) THEN
        INDX = 4
        IUNKG = 1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        DO 38 N = 1,NFX
          DVAR(N) = 0.D+0
          IF( IOM.EQ.30 .OR. IOM.EQ.40 ) THEN
            DVAR(N) = DVAR(N) +
     &        VAR*PORD(2,N)*VOL(N)*
     &        (XLC(IPNVGC,2,N)*SL(2,N)*RHOL(2,N) +
     &         XGC(IPNVGC,2,N)*SG(2,N)*RHOG(2,N))
          ELSEIF( IOM.EQ.43 ) THEN
            DVAR(N) = DVAR(N) +
     &        VAR*PORD(2,N)*VOL(N)*
     &        ( XLC(IPNVGC,2,N)*SL(2,N)*RHOL(2,N) +
     &        XGC(IPNVGC,2,N)*SG(2,N)*RHOG(2,N) +
     &        XNC(IPNVGC,2,N)*SN(2,N)*RHON(2,N) )
          ELSEIF( IOM.GE.36 .AND. IOM.LE.39 ) THEN
            DVAR(N) = DVAR(N) +
     &        VAR*PORD(2,N)*VOL(N)*
     &        (XLA(2,N)*SL(2,N)*RHOL(2,N) +
     &         XGA(2,N)*SG(2,N)*RHOG(2,N) +
     &         XHA(2,N)*SH(2,N)*RHOH(2,N))
          ELSE
            DVAR(N) = DVAR(N) +
     &        VAR*PORD(2,N)*VOL(N)*XLA(2,N)*SL(2,N)*RHOL(2,N)
            IF( IEQA.GT.0 ) DVAR(N) = DVAR(N) +
     &        VAR*PORD(2,N)*VOL(N)*XGA(2,N)*SG(2,N)*RHOG(2,N)
          ENDIF
   38   CONTINUE
        IF( IOM.EQ.30 .OR. IOM.EQ.40 .OR. IOM.EQ.43 ) THEN
          NCHX = INDEX( GCNM(IPNVGC)(1:),'  ' ) - 1
          WRITE(IPL,'(/,2A)') 'Total ' // GCNM(IPNVGC)(1:NCHX)
     &      // ' Mass, ',UNPLOT(IPNV)
        ELSEIF( IOM.GE.32 .AND. IOM.LE.39 ) THEN
          WRITE(IPL,'(/,2A)') 'Total CO2 Mass, ',UNPLOT(IPNV)
        ELSE
          WRITE(IPL,'(/,2A)') 'Total Air Mass, ',UNPLOT(IPNV)
        ENDIF
        WRITE(IPL,FORM) (DVAR(N),N=1,NFX)
      ELSEIF( IPNV.EQ.39 ) THEN
        INDX = 4
        IUNKG = 1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        DO 39 N = 1,NFX
          DVAR(N) = 0.D+0
          IF( IEQW.GT.0 ) DVAR(N) = DVAR(N) +
     &      VAR*PORD(2,N)*VOL(N)*XLO(2,N)*SL(2,N)*RHOL(2,N)
          IF( IEQA.GT.0 ) DVAR(N) = DVAR(N) +
     &      VAR*PORD(2,N)*VOL(N)*XGO(2,N)*SG(2,N)*RHOG(2,N)
          IF( IEQO.GT.0 ) DVAR(N) = DVAR(N) +
     &      VAR*PORD(2,N)*VOL(N)*SN(2,N)*RHON(2,N) +
     &      VAR*(1.D+0-PORT(2,N))*VOL(N)*XSO(2,N)*RHOS(IZ(N))
          IF( IOM.EQ.7 ) DVAR(N) = DVAR(N) +
     &      VAR*PORD(2,N)*VOL(N)*XNO(2,N)*SN(2,N)*RHON(2,N)
          IF( IOM.GE.36 .AND. IOM.LE.39 ) DVAR(N) = DVAR(N) +
     &      VAR*PORD(2,N)*VOL(N)*XHO(2,N)*SH(2,N)*RHOH(2,N)
   39   CONTINUE
        WRITE(IPL,'(/,2A)') 'Total Oil Mass, ',UNPLOT(IPNV)
        WRITE(IPL,FORM) (DVAR(N),N=1,NFX)
      ELSEIF( IPNV.EQ.40 ) THEN
        INDX = 4
        IUNKG = 1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Water Mass Source Integral, ',
     &    UNPLOT(IPNV)
        WRITE(IPL,FORM) (VAR*SRCIW(N),N=1,NFX)
      ELSEIF( IPNV.EQ.41 ) THEN
        INDX = 4
        IUNKG = 1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        IF( IOM.EQ.30 .OR. IOM.EQ.40 .OR. IOM.EQ.43 ) THEN
          NCHX = INDEX( GCNM(IPNVGC)(1:),'  ' ) - 1
          WRITE(IPL,'(/,A)') GCNM(IPNVGC)(1:NCHX) // 
     &      ' Mass Source Integral, ' // UNPLOT(IPNV)
          WRITE(IPL,FORM) (VAR*SRCIGC(IPNVGC,N),N=1,NFX)
        ELSEIF( IOM.GE.32 .AND. IOM.LE.39 ) THEN
          WRITE(IPL,'(/,2A)') 'CO2 Mass Source Integral, ',
     &      UNPLOT(IPNV)
          WRITE(IPL,FORM) (VAR*SRCIA(N),N=1,NFX)
        ELSE
          WRITE(IPL,'(/,2A)') 'Air Mass Source Integral, ',
     &      UNPLOT(IPNV)
          WRITE(IPL,FORM) (VAR*SRCIA(N),N=1,NFX)
        ENDIF
      ELSEIF( IPNV.EQ.42 ) THEN
        INDX = 4
        IUNKG = 1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Oil Mass Source Integral, ',
     &    UNPLOT(IPNV)
        WRITE(IPL,FORM) (VAR*SRCIO(N),N=1,NFX)
      ELSEIF( IPNV.EQ.43 ) THEN
        INDX = 4
        IUNM = 2
        IUNKG = 1
        IUNS = -2
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Thermal Energy Source Integral, ',
     &    UNPLOT(IPNV)
        WRITE(IPL,FORM) (VAR*SRCIT(N),N=1,NFX)
      ELSEIF( IPNV.EQ.44 ) THEN
        INDX = 4
        IUNM = 1
        IUNKG = 1
        IUNS = -3
        IUNK = -1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        JNDX = 1
        DO 44 N = 1,NFX
          IF( ISLC(5).EQ.1 ) THEN
            DVAR(N) = VAR*THKE( IZ(N),SL(2,N),SN(2,N),SI(2,N),
     &          THKL(2,N),THKG(2,N),THKN(2,N),THKI(2,N),
     &          PORD(2,N),PORT(2,N),JNDX )
          ELSE
            DVAR(N) = VAR*THKE( IZ(N),SL(2,N),SN(2,N),ZERO,
     &          THKL(2,N),THKG(2,N),THKN(2,N),ZERO,
     &          PORD(2,N),PORT(2,N),JNDX )
          ENDIF
   44   CONTINUE
        WRITE(IPL,'(/,2A)') 'X-Dir. Thermal Conductivity, ',
     &    UNPLOT(IPNV)
        WRITE(IPL,FORM) (DVAR(N),N=1,NFX)
      ELSEIF( IPNV.EQ.45 ) THEN
        INDX = 4
        IUNM = 1
        IUNKG = 1
        IUNS = -3
        IUNK = -1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        JNDX = 2
        DO 45 N = 1,NFX
          IF( ISLC(5).EQ.1 ) THEN
            DVAR(N) = VAR*THKE( IZ(N),SL(2,N),SN(2,N),SI(2,N),
     &          THKL(2,N),THKG(2,N),THKN(2,N),THKI(2,N),
     &          PORD(2,N),PORT(2,N),JNDX )
          ELSE
            DVAR(N) = VAR*THKE( IZ(N),SL(2,N),SN(2,N),ZERO,
     &          THKL(2,N),THKG(2,N),THKN(2,N),ZERO,
     &          PORD(2,N),PORT(2,N),JNDX )
          ENDIF
   45   CONTINUE
        WRITE(IPL,'(/,2A)') 'Y-Dir. Thermal Conductivity, ',
     &    UNPLOT(IPNV)
        WRITE(IPL,FORM) (DVAR(N),N=1,NFX)
      ELSEIF( IPNV.EQ.46 ) THEN
        INDX = 4
        IUNM = 1
        IUNKG = 1
        IUNS = -3
        IUNK = -1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        JNDX = 3
        DO 46 N = 1,NFX
          IF( ISLC(5).EQ.1 ) THEN
            DVAR(N) = VAR*THKE( IZ(N),SL(2,N),SN(2,N),SI(2,N),
     &          THKL(2,N),THKG(2,N),THKN(2,N),THKI(2,N),
     &          PORD(2,N),PORT(2,N),JNDX )
          ELSE
            DVAR(N) = VAR*THKE( IZ(N),SL(2,N),SN(2,N),ZERO,
     &          THKL(2,N),THKG(2,N),THKN(2,N),ZERO,
     &          PORD(2,N),PORT(2,N),JNDX )
          ENDIF
   46   CONTINUE
        WRITE(IPL,'(/,2A)') 'Z-Dir. Thermal Conductivity, ',
     &    UNPLOT(IPNV)
        WRITE(IPL,FORM) (DVAR(N),N=1,NFX)
      ELSEIF( IPNV.EQ.47 ) THEN
        INDX = 4
        IUNM = -3
        IUNKG = 1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)')'Salt Volumetric Concentration, ',
     &    UNPLOT(IPNV)
        WRITE(IPL,FORM) (VAR*TMS(2,N),N=1,NFX)
      ELSEIF( IPNV.EQ.48 ) THEN
        INDX = 4
        IUNM = -3
        IUNKG = 1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        DO 48 N = 1,NFX
          DVAR(N) = VAR*XLS(2,N)*RHOL(2,N)
   48   CONTINUE
        WRITE(IPL,'(/,2A)') 'Salt Aqueous Concentration, ',
     &    UNPLOT(IPNV)
        WRITE(IPL,FORM) (DVAR(N),N=1,NFX)
      ENDIF
!
!---  Deallocate memory  ---
!
      DEALLOCATE( DVAR,STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Deallocation Error: DVAR'
        CALL WRMSGS( INDX )
      ENDIF
      ISUB_LOG = ISUB_LOG-1
!
!---  End of WRPL_3 group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE WRPL_4( FORM,IPNV,IPNVGC,NFX )
!
!-------------------------Disclaimer-----------------------------------!
!
!     This material was prepared as an account of work sponsored by
!     an agency of the United States Government. Neither the
!     United States Government nor the United States Department of
!     Energy, nor Battelle, nor any of their employees, makes any
!     warranty, express or implied, or assumes any legal liability or
!     responsibility for the accuracy, completeness, or usefulness
!     of any information, apparatus, product, software or process
!     disclosed, or represents that its use would not infringe
!     privately owned rights.
!
!----------------------Acknowledgement---------------------------------!
!
!     This software and its documentation were produced with Government
!     support under Contract Number DE-AC06-76RLO-1830 awarded by the
!     United Department of Energy. The Government retains a paid-up
!     non-exclusive, irrevocable worldwide license to reproduce,
!     prepare derivative works, perform publicly and display publicly
!     by or for the Government, including the right to distribute to
!     other Government contractors.
!
!---------------------Copyright Notices--------------------------------!
!
!            Copyright Battelle Memorial Institute, 1996
!                    All Rights Reserved.
!
!----------------------Description-------------------------------------!
!
!     Write plot files for the following variables:
!
!     49               Maximum local aqueous Courant number
!     50               Total salt mass
!     51               X-Dir. aqueous Darcy velocity
!     52               Y-Dir. aqueous Darcy velocity
!     53               Z-Dir. aqueous Darcy velocity
!     54               X-Dir. gas Darcy velocity
!     55               Y-Dir. gas Darcy velocity
!     56               Z-Dir. gas Darcy velocity
!     57               X-Dir. NAPL Darcy velocity
!     58               Y-Dir. NAPL Darcy velocity
!     59               Z-Dir. NAPL Darcy velocity
!     60               X-Dir. heat flux
!     61               Y-Dir. heat flux
!     62               Z-Dir. heat flux
!     63               Matric potential head
!                      or Webb matching point head
!     64               X-Dir. salt flux
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 1 January 2003.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE TRNSPT
      USE SOLTN
      USE PORMED
      USE OUTPU
      USE GRID
      USE FLUXT
      USE FLUXS
      USE FLUXP
      USE FLUXN
      USE FILES
      USE FDVP
      USE CONST
!

!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Parameter Statements----------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*16 FORM
      REAL*8, DIMENSION(:), ALLOCATABLE ::  DVAR
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/WRPL_4'
      IF( INDEX(SVN_ID(277)(1:1),'$').EQ.0 ) SVN_ID(277) =
     & '$Id: wrplot.F 1080 2017-03-14 16:22:02Z d3c002 $' 
!
!---  Dynamic memory allocation  ---
!
      ALLOCATE( DVAR(1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DVAR'
        CALL WRMSGS( INDX )
      ENDIF
      VAR = 1.D+0
      IPNVGCX = IPNVGC
      IF( IPNV.EQ.49 ) THEN
        WRITE(IPL,'(/,A)') 'Maximum Local Aqueous Courant Number '
        WRITE(IPL,FORM) (CRNTL(N),N=1,NFX)
      ELSEIF( IPNV.EQ.50 ) THEN
        INDX = 4
        IUNKG = 1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        DO 50 N = 1,NFX
          DVAR(N) = VAR*TMS(2,N)*VOL(N)
   50   CONTINUE
        WRITE(IPL,'(/,2A)') 'Total Salt Mass, ',
     &    UNPLOT(IPNV)
        WRITE(IPL,FORM) (DVAR(N),N=1,NFX)
      ELSEIF( IPNV.EQ.51 ) THEN
        INDX = 4
        IUNM = 1
        IUNS = -1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)')
     &    'X-Dir. Aqueous Darcy Velocity, ',UNPLOT(IPNV)
        NX = (IFLD+1)*JFLD*KFLD
        WRITE(IPL,FORM) (VAR*UL(1,N),N=1,NX)
      ELSEIF( IPNV.EQ.52 ) THEN
        INDX = 4
        IUNM = 1
        IUNS = -1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)')
     &    'Y-Dir. Aqueous Darcy Velocity, ',UNPLOT(IPNV)
        NY = IFLD*(JFLD+1)*KFLD
        WRITE(IPL,FORM) (VAR*VL(1,N),N=1,NY)
      ELSEIF( IPNV.EQ.53 ) THEN
        INDX = 4
        IUNM = 1
        IUNS = -1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)')
     &    'Z-Dir. Aqueous Darcy Velocity, ',UNPLOT(IPNV)
        NZ = IFLD*JFLD*(KFLD+1)
        WRITE(IPL,FORM) (VAR*WL(1,N),N=1,NZ)
      ELSEIF( IPNV.EQ.54 ) THEN
        INDX = 4
        IUNM = 1
        IUNS = -1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)')
     &    'X-Dir. Gas Darcy Velocity, ',UNPLOT(IPNV)
        NX = (IFLD+1)*JFLD*KFLD
        WRITE(IPL,FORM) (VAR*UG(1,N),N=1,NX)
      ELSEIF( IPNV.EQ.55 ) THEN
        INDX = 4
        IUNM = 1
        IUNS = -1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)')
     &    'Y-Dir. Gas Darcy Velocity, ',UNPLOT(IPNV)
        NY = IFLD*(JFLD+1)*KFLD
        WRITE(IPL,FORM) (VAR*VG(1,N),N=1,NY)
      ELSEIF( IPNV.EQ.56 ) THEN
        INDX = 4
        IUNM = 1
        IUNS = -1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)')
     &    'Z-Dir. Gas Darcy Velocity, ',UNPLOT(IPNV)
        NZ = IFLD*JFLD*(KFLD+1)
        WRITE(IPL,FORM) (VAR*WG(1,N),N=1,NZ)
      ELSEIF( IPNV.EQ.57 ) THEN
        INDX = 4
        IUNM = 1
        IUNS = -1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)')
     &    'X-Dir. NAPL Darcy Velocity, ',UNPLOT(IPNV)
        NX = (IFLD+1)*JFLD*KFLD
        WRITE(IPL,FORM) (VAR*UN(1,N),N=1,NX)
      ELSEIF( IPNV.EQ.58 ) THEN
        INDX = 4
        IUNM = 1
        IUNS = -1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)')
     &    'Y-Dir. NAPL Darcy Velocity, ',UNPLOT(IPNV)
        NY = IFLD*(JFLD+1)*KFLD
        WRITE(IPL,FORM) (VAR*VN(1,N),N=1,NY)
      ELSEIF( IPNV.EQ.59 ) THEN
        INDX = 4
        IUNM = 1
        IUNS = -1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)')
     &    'Z-Dir. NAPL Darcy Velocity, ',UNPLOT(IPNV)
        NZ = IFLD*JFLD*(KFLD+1)
        WRITE(IPL,FORM) (VAR*WN(1,N),N=1,NZ)
      ELSEIF( IPNV.EQ.60 ) THEN
        INDX = 4
        IUNKG = 1
        IUNS = -3
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'X-Dir. Heat Flux, ',UNPLOT(IPNV)
        NX = (IFLD+1)*JFLD*KFLD
        WRITE(IPL,FORM) (VAR*UQ(1,N),N=1,NX)
      ELSEIF( IPNV.EQ.61 ) THEN
        INDX = 4
        IUNKG = 1
        IUNS = -3
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Y-Dir. Heat Flux, ',UNPLOT(IPNV)
        NY = IFLD*(JFLD+1)*KFLD
        WRITE(IPL,FORM) (VAR*VQ(1,N),N=1,NY)
      ELSEIF( IPNV.EQ.62 ) THEN
        INDX = 4
        IUNKG = 1
        IUNS = -3
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Z-Dir. Heat Flux, ',UNPLOT(IPNV)
        NZ = IFLD*JFLD*(KFLD+1)
        WRITE(IPL,FORM) (VAR*WQ(1,N),N=1,NZ)
      ELSEIF( IPNV.EQ.63 ) THEN
        INDX = 4
        IUNM = 1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        IF( IOM.EQ.32 .OR. IOM.EQ.33 ) THEN
          WRITE(IPL,'(/,2A)') 'Webb Matching Point Head, ',UNPLOT(IPNV)
          WRITE(IPL,FORM) (VAR*SCHR(9,IZ(N)),N=1,NFX)
        ELSE
          WRITE(IPL,'(/,2A)') 'Matric Potential Head, ',UNPLOT(IPNV)
          WRITE(IPL,FORM) (VAR*(MIN(PL(2,N)-PG(2,N),0.D+0))
     &      /RHORL/GRAV,N=1,NFX)
        ENDIF
      ELSEIF( IPNV.EQ.64 ) THEN
        INDX = 4
        IUNKG = 1
        IUNM = -2
        IUNS = -1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'X-Dir. Salt Flux, ',
     &    UNPLOT(IPNV)
        NX = (IFLD+1)*JFLD*KFLD
        WRITE(IPL,FORM) (VAR*US(1,N),N=1,NX)
      ENDIF
!
!---  Dynamic memory allocation  ---
!
      DEALLOCATE( DVAR,STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Deallocation Error: DVAR'
        CALL WRMSGS( INDX )
      ENDIF
      ISUB_LOG = ISUB_LOG-1
!
!---  End of WRPL_4 group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE WRPL_5( FORM,IPNV,IPNVGC,NFX )
!
!-------------------------Disclaimer-----------------------------------!
!
!     This material was prepared as an account of work sponsored by
!     an agency of the United States Government. Neither the
!     United States Government nor the United States Department of
!     Energy, nor Battelle, nor any of their employees, makes any
!     warranty, express or implied, or assumes any legal liability or
!     responsibility for the accuracy, completeness, or usefulness
!     of any information, apparatus, product, software or process
!     disclosed, or represents that its use would not infringe
!     privately owned rights.
!
!----------------------Acknowledgement---------------------------------!
!
!     This software and its documentation were produced with Government
!     support under Contract Number DE-AC06-76RLO-1830 awarded by the
!     United Department of Energy. The Government retains a paid-up
!     non-exclusive, irrevocable worldwide license to reproduce,
!     prepare derivative works, perform publicly and display publicly
!     by or for the Government, including the right to distribute to
!     other Government contractors.
!
!---------------------Copyright Notices--------------------------------!
!
!            Copyright Battelle Memorial Institute, 1996
!                    All Rights Reserved.
!
!----------------------Description-------------------------------------!
!
!     Write plot files for the following variables:
!
!     65               Y-Dir. salt flux
!     66               Z-Dir. salt flux
!     67               X-Dir. salt flux (node centered)
!     68               Y-Dir. salt flux (node centered)
!     69               Z-Dir. salt flux (node centered)
!     70               Gas water mole fraction
!     71               Gas air mole fraction
!     71    IOM=30     Gas gas component mole fraction
!     71    IOM=40     Gas gas component mole fraction
!     72               Gas oil mole fraction
!     72    IOM=43     Nonaqueous-liquid component mole fraction
!     73               Gas water concentration
!     74               Gas air concentration
!     74    IOM=30     Gas gas component concentration
!     74    IOM=40     Gas gas component concentration
!     75               Gas oil concentration
!     76               Aqueous water concentration
!     77               Aqueous air concentration
!     77    IOM=30     Aqueous gas concentration
!     77    IOM=40     Aqueous gas concentration
!     78               Aqueous oil concentration
!     79               Gas Courant number
!     80               Ice pressure
!     80    IOM=43     System pressure
!     80    ISLC(50)=1 Stress-xx
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 8 January 2003.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE TRNSPT
      USE SOLTN
      USE OUTPU
      USE GRID
      USE GEOMECH
      USE FLUXS
      USE FILES
      USE FDVP
      USE FDVGC
      USE CONST
!

!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Parameter Statements----------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*16 FORM
      REAL*8, DIMENSION(:), ALLOCATABLE ::  DVAR
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/WRPL_5'
      IF( INDEX(SVN_ID(277)(1:1),'$').EQ.0 ) SVN_ID(277) =
     & '$Id: wrplot.F 1080 2017-03-14 16:22:02Z d3c002 $' 
!
!---  Dynamic memory allocation  ---
!
      ALLOCATE( DVAR(1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DVAR'
        CALL WRMSGS( INDX )
      ENDIF
      VAR = 1.D+0
      IF( IPNV.EQ.65 ) THEN
        INDX = 4
        IUNKG = 1
        IUNM = -2
        IUNS = -1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Y-Dir. Salt Flux, ',
     &    UNPLOT(IPNV)
        NY = IFLD*(JFLD+1)*KFLD
        WRITE(IPL,FORM) (VAR*VS(1,N),N=1,NY)
      ELSEIF( IPNV.EQ.66 ) THEN
        INDX = 4
        IUNKG = 1
        IUNM = -2
        IUNS = -1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Z-Dir. Salt Flux, ',
     &    UNPLOT(IPNV)
        NZ = IFLD*JFLD*(KFLD+1)
        WRITE(IPL,FORM) (VAR*WS(1,N),N=1,NZ)
      ELSEIF( IPNV.EQ.67 ) THEN
        INDX = 4
        IUNKG = 1
        IUNM = -2
        IUNS = -1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'X-Dir. Salt Flux (Node Centered)' //
     &    ', ',UNPLOT(IPNV)
        DO N = 1,NFX
          IF( IXP(N).EQ.0 ) THEN
            DVAR(N) = 0.D+0
          ELSE
            DVAR(N) = VAR*0.5D+0*(US(1,NSX(N))+US(1,NSX(N)+1))
          ENDIF
        ENDDO
        WRITE(IPL,FORM) (DVAR(N),N=1,NFX)
      ELSEIF( IPNV.EQ.68 ) THEN
        INDX = 4
        IUNKG = 1
        IUNM = -2
        IUNS = -1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Y-Dir. Salt Flux (Node Centered)' //
     &    ', ',UNPLOT(IPNV)
        DO N = 1,NFX
          IF( IXP(N).EQ.0 ) THEN
            DVAR(N) = 0.D+0
          ELSE
            DVAR(N) = VAR*0.5D+0*(VS(1,NSY(N))+VS(1,NSY(N)+IFLD))
          ENDIF
        ENDDO
        WRITE(IPL,FORM) (DVAR(N),N=1,NFX)
      ELSEIF( IPNV.EQ.69 ) THEN
        INDX = 4
        IUNKG = 1
        IUNM = -2
        IUNS = -1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Z-Dir. Salt Flux (Node Centered)' //
     &    ', ',UNPLOT(IPNV)
        DO N = 1,NFX
          IF( IXP(N).EQ.0 ) THEN
            DVAR(N) = 0.D+0
          ELSE
            DVAR(N) = VAR*0.5D+0*(WS(1,NSZ(N))+WS(1,NSZ(N)+IJFLD))
          ENDIF
        ENDDO
        WRITE(IPL,FORM) (DVAR(N),N=1,NFX)
      ELSEIF( IPNV.EQ.70 ) THEN
        WRITE(IPL,'(/,A)') 'Gas Water Mole Fraction'
        WRITE(IPL,FORM) (XMGW(2,N),N=1,NFX)
      ELSEIF( IPNV.EQ.71 ) THEN
        IF( IOM.EQ.30 .OR. IOM.EQ.40 .OR. IOM.EQ.43 .OR. 
     &    IOM.EQ.51 ) THEN
          NCHX = INDEX(GCNM(IPNVGC)(1:),'  ')-1
          WRITE(IPL,'(/,A)') 'Gas ' // GCNM(IPNVGC)(1:NCHX)
     &      // ' Mole Fraction'
          WRITE(IPL,FORM) (XMGC(IPNVGC,2,N),N=1,NFX)
        ELSEIF( IOM.GE.32 .AND. IOM.LE.39 ) THEN
          WRITE(IPL,'(/,A)') 'Gas CO2 Mole Fraction'
          WRITE(IPL,FORM) (XMGA(2,N),N=1,NFX)
        ELSE
          WRITE(IPL,'(/,A)') 'Gas Air Mole Fraction'
          WRITE(IPL,FORM) (XMGA(2,N),N=1,NFX)
        ENDIF
      ELSEIF( IPNV.EQ.72 ) THEN
        IF( IOM.EQ.51 ) THEN
          NCHX = INDEX(GCNM(IPNVGC)(1:),'  ')-1
          WRITE(IPL,'(/,A)') 'Liquid ' // GCNM(IPNVGC)(1:NCHX)
     &      // ' Mole Fraction'
          WRITE(IPL,FORM) (XMNC(IPNVGC,2,N),N=1,NFX)
        ELSEIF( IOM.EQ.43 ) THEN
          NCHX = INDEX(GCNM(IPNVGC)(1:),'  ')-1
          WRITE(IPL,'(/,A)') 'Nonaqueous-Liquid ' // 
     &      GCNM(IPNVGC)(1:NCHX) // ' Mole Fraction'
          WRITE(IPL,FORM) (XMNC(IPNVGC,2,N),N=1,NFX)
        ELSE
          WRITE(IPL,'(/,A)') 'Gas Oil Mole Fraction'
          WRITE(IPL,FORM) (XMGO(2,N),N=1,NFX)
        ENDIF
      ELSEIF( IPNV.EQ.73 ) THEN
        INDX = 4
        IUNM = -3
        IUNKG = 1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)')
     &    'Gas Water Concentration, ',UNPLOT(IPNV)
        WRITE(IPL,FORM) (VAR*XGW(2,N)*RHOG(2,N),N=1,NFX)
      ELSEIF( IPNV.EQ.74 ) THEN
        INDX = 4
        IUNM = -3
        IUNKG = 1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        IF( IOM.EQ.30 .OR. IOM.EQ.40 .OR. IOM.EQ.43 .OR. 
     &    IOM.EQ.51 ) THEN
          NCHX = INDEX(GCNM(IPNVGC)(1:),'  ')-1
          WRITE(IPL,'(/,A)') 'Gas Mass ' // GCNM(IPNVGC)(1:NCHX)
     &      // ' Concentration, ' // UNPLOT(IPNV)
          WRITE(IPL,FORM) (VAR*XGC(IPNVGC,2,N)*RHOG(2,N),N=1,NFX)
        ELSEIF( IOM.GE.32 .AND. IOM.LE.39 ) THEN
          WRITE(IPL,'(/,A)')
     &      'Gas CO2 Concentration, ' // UNPLOT(IPNV)
          WRITE(IPL,FORM) (VAR*XGA(2,N)*RHOG(2,N),N=1,NFX)
        ELSE
          WRITE(IPL,'(/,A)')
     &      'Gas Air Concentration, ' // UNPLOT(IPNV)
          WRITE(IPL,FORM) (VAR*XGA(2,N)*RHOG(2,N),N=1,NFX)
        ENDIF
      ELSEIF( IPNV.EQ.75 ) THEN
        INDX = 4
        IUNM = -3
        IUNKG = 1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        IF( IOM.EQ.51 ) THEN
          NCHX = INDEX(GCNM(IPNVGC)(1:),'  ')-1
          WRITE(IPL,'(/,A)') 'Liquid Mass ' // GCNM(IPNVGC)(1:NCHX)
     &      // ' Concentration, ' // UNPLOT(IPNV)
          WRITE(IPL,FORM) (VAR*XNC(IPNVGC,2,N)*RHON(2,N),N=1,NFX)
        ELSEIF( IOM.GE.36 .AND. IOM.LE.39 ) THEN
          WRITE(IPL,'(/,2A)')
     &      'Gas CH4 Concentration, ',UNPLOT(IPNV)
        ELSE
          WRITE(IPL,'(/,2A)')
     &      'Gas Oil Concentration, ',UNPLOT(IPNV)
        ENDIF
        WRITE(IPL,FORM) (VAR*XGO(2,N)*RHOG(2,N),N=1,NFX)
      ELSEIF( IPNV.EQ.76 ) THEN
        INDX = 4
        IUNM = -3
        IUNKG = 1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)')
     &    'Aqueous Water Concentration, ',UNPLOT(IPNV)
        WRITE(IPL,FORM) (VAR*XLW(2,N)*RHOL(2,N),N=1,NFX)
      ELSEIF( IPNV.EQ.77 ) THEN
        INDX = 4
        IUNM = -3
        IUNKG = 1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        IF( IOM.EQ.30 .OR. IOM.EQ.40 .OR. IOM.EQ.43 ) THEN
          NCHX = INDEX(GCNM(IPNVGC)(1:),'  ')-1
          WRITE(IPL,'(/,A)') 'Aqueous ' // GCNM(IPNVGC)(1:NCHX)
     &      // ' Concentration, ' // UNPLOT(IPNV)
          WRITE(IPL,FORM) (VAR*XLC(IPNVGC,2,N)*RHOL(2,N),N=1,NFX)
        ELSEIF( IOM.GE.32 .AND. IOM.LE.39 ) THEN
          WRITE(IPL,'(/,A)')
     &      'Aqueous CO2 Concentration, ' // UNPLOT(IPNV)
          WRITE(IPL,FORM) (VAR*XLA(2,N)*RHOL(2,N),N=1,NFX)
        ELSE
          WRITE(IPL,'(/,A)')
     &      'Aqueous Air Concentration, ' // UNPLOT(IPNV)
          WRITE(IPL,FORM) (VAR*XLA(2,N)*RHOL(2,N),N=1,NFX)
        ENDIF
      ELSEIF( IPNV.EQ.78 ) THEN
        INDX = 4
        IUNM = -3
        IUNKG = 1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        IF( IOM.GE.36 .AND. IOM.LE.39 ) THEN
          WRITE(IPL,'(/,2A)')
     &      'Aqueous CH4 Concentration, ',UNPLOT(IPNV)
        ELSE
          WRITE(IPL,'(/,2A)')
     &      'Aqueous Oil Concentration, ',UNPLOT(IPNV)
        ENDIF
        WRITE(IPL,FORM) (VAR*XLO(2,N)*RHOL(2,N),N=1,NFX)
      ELSEIF( IPNV.EQ.79 ) THEN
        WRITE(IPL,'(/,A)') 'Maximum Local Gas Courant Number '
        WRITE(IPL,FORM) (CRNTG(N),N=1,NFX)
      ELSEIF( IPNV.EQ.80 ) THEN
        INDX = 4
        IUNM = -1
        IUNKG = 1
        IUNS = -2
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
!
!---    Geomechanics  ---
!
        IF( ISLC(50).NE.0 ) THEN
          WRITE(IPL,'(/,2A)') 'Stress-xx, ',UNPLOT(IPNV)
          WRITE(IPL,FORM) (VAR*SIG_GM(1,N),N=1,NFX)
!
!---    STOMP-EOR  ---
!
        ELSEIF( IOM.EQ.43 ) THEN
          WRITE(IPL,'(/,2A)') 'System Pressure, ',UNPLOT(IPNV)
          WRITE(IPL,FORM) (VAR*(PSO(2,N)+PATM),N=1,NFX)
        ELSE
          WRITE(IPL,'(/,2A)') 'Ice Pressure, ',UNPLOT(IPNV)
          WRITE(IPL,FORM) (VAR*(PI(2,N)+PATM),N=1,NFX)
        ENDIF
      ENDIF
!
!---  Deallocate memory  ---
!
      DEALLOCATE( DVAR,STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Deallocation Error: DVAR'
        CALL WRMSGS( INDX )
      ENDIF
      ISUB_LOG = ISUB_LOG-1
!
!---  End of WRPL_5 group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE WRPL_6( FORM,IPNV,IPNVGC,NFX )
!
!-------------------------Disclaimer-----------------------------------!
!
!     This material was prepared as an account of work sponsored by
!     an agency of the United States Government. Neither the
!     United States Government nor the United States Department of
!     Energy, nor Battelle, nor any of their employees, makes any
!     warranty, express or implied, or assumes any legal liability or
!     responsibility for the accuracy, completeness, or usefulness
!     of any information, apparatus, product, software or process
!     disclosed, or represents that its use would not infringe
!     privately owned rights.
!
!----------------------Acknowledgement---------------------------------!
!
!     This software and its documentation were produced with Government
!     support under Contract Number DE-AC06-76RLO-1830 awarded by the
!     United Department of Energy. The Government retains a paid-up
!     non-exclusive, irrevocable worldwide license to reproduce,
!     prepare derivative works, perform publicly and display publicly
!     by or for the Government, including the right to distribute to
!     other Government contractors.
!
!---------------------Copyright Notices--------------------------------!
!
!            Copyright Battelle Memorial Institute, 1996
!                    All Rights Reserved.
!
!----------------------Description-------------------------------------!
!
!     Write plot files for the following variables:
!
!     81 ice saturation 'SI '
!     82 ice density 'RHOF'
!     83 aqueous matrix (null), 'DSLM'
!       or NAPL matrix (null), 'DSNM'
!       or Eclipse gas saturation (null), 'ESG'
!     84 aqueous fracture (null), 'DSLF'
!       or NAPL fracture (null), 'DSNF'
!       or Eclipse oil saturation (null), 'ESO'
!     85 gas matrix 'DSGM'
!     86 gas fracture 'DSGF'
!     87 xnc aqueous volumetric flux 'ULNC'
!     88 ync aqueous volumetric flux 'VLNC'
!     89 znc aqueous volumetric flux 'WLNC'
!     90 xnc gas volumetric flux 'UGNC'
!     91 ync gas volumetric flux 'VGNC'
!     92 znc gas volumetric flux 'WGNC'
!     93 xnc NAPL volumetric flux 'UNNC'
!     94 ync NAPL volumetric flux 'VNNC'
!     95 znc NAPL volumetric flux 'WNNC'
!     96 xnc heat flux 'UQNC'
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 31 December 2003.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE OUTPU
      USE GRID
      USE FLUXT
      USE FLUXP
      USE FLUXN
      USE FILES
      USE FDVP
      USE FDVI
!

!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Parameter Statements----------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*16 FORM
      REAL*8, DIMENSION(:), ALLOCATABLE ::  DVAR
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/WRPL_6'
      IF( INDEX(SVN_ID(277)(1:1),'$').EQ.0 ) SVN_ID(277) =
     & '$Id: wrplot.F 1080 2017-03-14 16:22:02Z d3c002 $' 
!
!---  Dynamic memory allocation  ---
!
      ALLOCATE( DVAR(1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DVAR'
        CALL WRMSGS( INDX )
      ENDIF
      VAR = 1.D+0
      IPNVGCX = IPNVGC
      IF( IPNV.EQ.81 ) THEN
        WRITE(IPL,'(/,A)') 'Ice-Phase Saturation'
        WRITE(IPL,FORM) (SI(2,N),N=1,NFX)
      ELSEIF( IPNV.EQ.82 ) THEN
        INDX = 4
        IUNM = -3
        IUNKG = 1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)')
     &    'Ice-Phase Density, ',UNPLOT(IPNV)
        WRITE(IPL,FORM) (VAR*RHOI(2,N),N=1,NFX)
      ELSEIF( IPNV.EQ.83 ) THEN
!
!---    STOMP-EOR  ---
!
        IF( IOM.EQ.43 ) THEN
          WRITE(IPL,'(/,A)') 'Eclipse Gas Saturation'
          WRITE(IPL,FORM) (SI(1,N),N=1,NFX)
        ELSE
          WRITE(IPL,'(/,A)') 'Dual Porosity Matrix Aqueous Saturation'
          WRITE(IPL,FORM) (SDPM(N),N=1,NFX)
        ENDIF
      ELSEIF( IPNV.EQ.84 ) THEN
!
!---    STOMP-EOR  ---
!
        IF( IOM.EQ.43 ) THEN
          WRITE(IPL,'(/,A)') 'Eclipse Oil Saturation'
          WRITE(IPL,FORM) (SI(2,N),N=1,NFX)
        ELSE
          WRITE(IPL,'(/,A)') 'Dual Porosity Fracture Aqueous Saturation'
          WRITE(IPL,FORM) (SDPF(N),N=1,NFX)
        ENDIF
      ELSEIF( IPNV.EQ.85 ) THEN
        WRITE(IPL,'(/,A)') 'Dual Porosity Matrix Gas Saturation'
        WRITE(IPL,FORM) ((1.D+0-SDPM(N)),N=1,NFX)
      ELSEIF( IPNV.EQ.86 ) THEN
        WRITE(IPL,'(/,A)') 'Dual Porosity Fracture Gas Saturation'
        WRITE(IPL,FORM) ((1.D+0-SDPF(N)),N=1,NFX)
      ELSEIF( IPNV.EQ.87 ) THEN
        INDX = 4
        IUNM = 1
        IUNS = -1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'X-Dir. Aqueous Darcy Velocity ' //
     &    '(Node Centered), ',UNPLOT(IPNV)
        DO N = 1,NFX
          IF( IXP(N).EQ.0 ) THEN
            DVAR(N) = 0.D+0
          ELSE
            DVAR(N) = VAR*0.5D+0*(UL(1,NSX(N))+UL(1,NSX(N)+1))
          ENDIF
        ENDDO
        WRITE(IPL,FORM) (DVAR(N),N=1,NFX)
      ELSEIF( IPNV.EQ.88 ) THEN
        INDX = 4
        IUNM = 1
        IUNS = -1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Y-Dir. Aqueous Darcy Velocity ' //
     &    '(Node Centered), ',UNPLOT(IPNV)
        DO N = 1,NFX
          IF( IXP(N).EQ.0 ) THEN
            DVAR(N) = 0.D+0
          ELSE
            DVAR(N) = VAR*0.5D+0*(VL(1,NSY(N))+VL(1,NSY(N)+IFLD))
          ENDIF
        ENDDO
        WRITE(IPL,FORM) (DVAR(N),N=1,NFX)
      ELSEIF( IPNV.EQ.89 ) THEN
        INDX = 4
        IUNM = 1
        IUNS = -1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Z-Dir. Aqueous Darcy Velocity ' //
     &    '(Node Centered), ',UNPLOT(IPNV)
        DO N = 1,NFX
          IF( IXP(N).EQ.0 ) THEN
            DVAR(N) = 0.D+0
          ELSE
            DVAR(N) = VAR*0.5D+0*(WL(1,NSZ(N))+WL(1,NSZ(N)+IJFLD))
          ENDIF
        ENDDO
        WRITE(IPL,FORM) (DVAR(N),N=1,NFX)
      ELSEIF( IPNV.EQ.90 ) THEN
        INDX = 4
        IUNM = 1
        IUNS = -1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'X-Dir. Gas Darcy Velocity ' //
     &    '(Node Centered), ',UNPLOT(IPNV)
        DO N = 1,NFX
          IF( IXP(N).EQ.0 ) THEN
            DVAR(N) = 0.D+0
          ELSE
            DVAR(N) = VAR*0.5D+0*(UG(1,NSX(N))+UG(1,NSX(N)+1))
          ENDIF
        ENDDO
        WRITE(IPL,FORM) (DVAR(N),N=1,NFX)
      ELSEIF( IPNV.EQ.91 ) THEN
        INDX = 4
        IUNM = 1
        IUNS = -1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Y-Dir. Gas Darcy Velocity ' //
     &    '(Node Centered), ',UNPLOT(IPNV)
        DO N = 1,NFX
          IF( IXP(N).EQ.0 ) THEN
            DVAR(N) = 0.D+0
          ELSE
            DVAR(N) = VAR*0.5D+0*(VG(1,NSY(N))+VG(1,NSY(N)+IFLD))
          ENDIF
        ENDDO
        WRITE(IPL,FORM) (DVAR(N),N=1,NFX)
      ELSEIF( IPNV.EQ.92 ) THEN
        INDX = 4
        IUNM = 1
        IUNS = -1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Z-Dir. Gas Darcy Velocity ' //
     &    '(Node Centered), ',UNPLOT(IPNV)
        DO N = 1,NFX
          IF( IXP(N).EQ.0 ) THEN
            DVAR(N) = 0.D+0
          ELSE
            DVAR(N) = VAR*0.5D+0*(WG(1,NSZ(N))+WG(1,NSZ(N)+IJFLD))
          ENDIF
        ENDDO
        WRITE(IPL,FORM) (DVAR(N),N=1,NFX)
      ELSEIF( IPNV.EQ.93 ) THEN
        INDX = 4
        IUNM = 1
        IUNS = -1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'X-Dir. NAPL Darcy Velocity ' //
     &    '(Node Centered), ',UNPLOT(IPNV)
        DO N = 1,NFX
          IF( IXP(N).EQ.0 ) THEN
            DVAR(N) = 0.D+0
          ELSE
            DVAR(N) = VAR*0.5D+0*(UN(1,NSX(N))+UN(1,NSX(N)+1))
          ENDIF
        ENDDO
        WRITE(IPL,FORM) (DVAR(N),N=1,NFX)
      ELSEIF( IPNV.EQ.94 ) THEN
        INDX = 4
        IUNM = 1
        IUNS = -1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Y-Dir. NAPL Darcy Velocity ' //
     &    '(Node Centered), ',UNPLOT(IPNV)
        DO N = 1,NFX
          IF( IXP(N).EQ.0 ) THEN
            DVAR(N) = 0.D+0
          ELSE
            DVAR(N) = VAR*0.5D+0*(VN(1,NSY(N))+VN(1,NSY(N)+IFLD))
          ENDIF
        ENDDO
        WRITE(IPL,FORM) (DVAR(N),N=1,NFX)
      ELSEIF( IPNV.EQ.95 ) THEN
        INDX = 4
        IUNM = 1
        IUNS = -1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Z-Dir. NAPL Darcy Velocity ' //
     &    '(Node Centered), ',UNPLOT(IPNV)
        DO N = 1,NFX
          IF( IXP(N).EQ.0 ) THEN
            DVAR(N) = 0.D+0
          ELSE
            DVAR(N) = VAR*0.5D+0*(WN(1,NSZ(N))+WN(1,NSZ(N)+IJFLD))
          ENDIF
        ENDDO
        WRITE(IPL,FORM) (DVAR(N),N=1,NFX)
      ELSEIF( IPNV.EQ.96 ) THEN
        INDX = 4
        IUNKG = 1
        IUNS = -3
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)')
     &    'X-Dir. Heat Flux (Node Centered), ',UNPLOT(IPNV)
        DO N = 1,NFX
          IF( IXP(N).EQ.0 ) THEN
            DVAR(N) = 0.D+0
          ELSE
            DVAR(N) = VAR*0.5D+0*(UQ(1,NSX(N))+UQ(1,NSX(N)+1))
          ENDIF
        ENDDO
        WRITE(IPL,FORM) (DVAR(N),N=1,NFX)
      ENDIF
!
!---  Deallocate memory  ---
!
      DEALLOCATE( DVAR,STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Deallocation Error: DVAR'
        CALL WRMSGS( INDX )
      ENDIF
      ISUB_LOG = ISUB_LOG-1
!
!---  End of WRPL_6 group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE WRPL_7( FORM,IPNV,IPNVGC,NFX )
!
!-------------------------Disclaimer-----------------------------------!
!
!     This material was prepared as an account of work sponsored by
!     an agency of the United States Government. Neither the
!     United States Government nor the United States Department of
!     Energy, nor Battelle, nor any of their employees, makes any
!     warranty, express or implied, or assumes any legal liability or
!     responsibility for the accuracy, completeness, or usefulness
!     of any information, apparatus, product, software or process
!     disclosed, or represents that its use would not infringe
!     privately owned rights.
!
!----------------------Acknowledgement---------------------------------!
!
!     This software and its documentation were produced with Government
!     support under Contract Number DE-AC06-76RLO-1830 awarded by the
!     United Department of Energy. The Government retains a paid-up
!     non-exclusive, irrevocable worldwide license to reproduce,
!     prepare derivative works, perform publicly and display publicly
!     by or for the Government, including the right to distribute to
!     other Government contractors.
!
!---------------------Copyright Notices--------------------------------!
!
!            Copyright Battelle Memorial Institute, 1996
!                    All Rights Reserved.
!
!----------------------Description-------------------------------------!
!
!     Write plot files for the following variables:
!
!     97 ync heat flux 'VQNC'
!     98 znc heat flux 'WQNC'
!     99 NAPL courant 'CRNN'
!     100   node number 'NODE'
!     101 osmotic pressure 'POSM'
!     101 stress-yy, Pa
!     102 osmotic efficiency factor 'OEC '
!     103 aqueous alcohol concentration 'CLA '
!     104 NAPL alcohol concentration 'CNA '
!     105 trapped gas saturation 'SGT '
!     106 trapped NAPL saturation 'SNT '
!     107 aqueous trapped gas 'SGTL'
!     108 NAPL trapped gas 'SGTN'
!     109 dissolved-aqueous oil concentration 'CLO '
!     110 salt aqueous mass fraction 'XLS '
!     111 surfactant volumetric concentration 'CS '
!     112 surfactant aqueous concentration 'CLS '
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 31 December 2003.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE TRNSPT
      USE SOLTN
      USE OUTPU
      USE HYST
      USE GRID
      USE GEOMECH
      USE FLUXT
      USE FILES
      USE FDVS
      USE FDVP
      USE FDVA
!

!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Parameter Statements----------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*16 FORM
      REAL*8, DIMENSION(:), ALLOCATABLE ::  DVAR
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/WRPL_7'
      IF( INDEX(SVN_ID(277)(1:1),'$').EQ.0 ) SVN_ID(277) =
     & '$Id: wrplot.F 1080 2017-03-14 16:22:02Z d3c002 $' 
!
!---  Dynamic memory allocation  ---
!
      ALLOCATE( DVAR(1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DVAR'
        CALL WRMSGS( INDX )
      ENDIF
      VAR = 1.D+0
      IPNVGCX = IPNVGC
      IF( IPNV.EQ.97 ) THEN
        INDX = 4
        IUNKG = 1
        IUNS = -3
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)')
     &    'Y-Dir. Heat Flux (Node Centered), ',UNPLOT(IPNV)
        DO N = 1,NFX
          IF( IXP(N).EQ.0 ) THEN
            DVAR(N) = 0.D+0
          ELSE
            DVAR(N) = VAR*0.5D+0*(VQ(1,NSY(N))+VQ(1,NSY(N)+IFLD))
          ENDIF
        ENDDO
        WRITE(IPL,FORM) (DVAR(N),N=1,NFX)
      ELSEIF( IPNV.EQ.98 ) THEN
        INDX = 4
        IUNKG = 1
        IUNS = -3
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)')
     &    'Z-Dir. Heat Flux (Node Centered), ',UNPLOT(IPNV)
        DO N = 1,NFX
          IF( IXP(N).EQ.0 ) THEN
            DVAR(N) = 0.D+0
          ELSE
            DVAR(N) = VAR*0.5D+0*(WQ(1,NSZ(N))+WQ(1,NSZ(N)+IJFLD))
          ENDIF
        ENDDO
        WRITE(IPL,FORM) (DVAR(N),N=1,NFX)
      ELSEIF( IPNV.EQ.99 ) THEN
        WRITE(IPL,'(/,A)') 'Maximum Local NAPL Courant Number '
        WRITE(IPL,FORM) (CRNTN(N),N=1,NFX)
      ELSEIF( IPNV.EQ.100 ) THEN
        WRITE(IPL,'(/,A)') 'Node Number'
        WRITE(IPL,FORM) (REAL(N),N=1,NFX)
      ELSEIF( IPNV.EQ.101 ) THEN
        INDX = 4
        IUNM = -1
        IUNKG = 1
        IUNS = -2
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
!
!---    Geomechanics  ---
!
        IF( ISLC(50).NE.0 ) THEN
          WRITE(IPL,'(/,2A)') 'Stress-yy, ',UNPLOT(IPNV)
          WRITE(IPL,FORM) (VAR*SIG_GM(2,N),N=1,NFX)
        ELSE
          WRITE(IPL,'(/,2A)')'Osmotic Pressure, ',UNPLOT(IPNV)
          WRITE(IPL,FORM) (VAR*POSM(2,N),N=1,NFX)
        ENDIF
      ELSEIF( IPNV.EQ.102 ) THEN
        WRITE(IPL,'(/,A)')'Osmotic Efficiency Coefficient '
        WRITE(IPL,FORM) (OEC(2,N),N=1,NFX)
      ELSEIF( IPNV.EQ.103 ) THEN
        WRITE(IPL,'(/,A)')'Aqueous Alcohol Concentration '
        WRITE(IPL,FORM) ((XLA(2,N)*RHOL(2,N)),N=1,NFX)
      ELSEIF( IPNV.EQ.104 ) THEN
        WRITE(IPL,'(/,A)')'NAPL Alcohol Concentration '
        WRITE(IPL,FORM) ((XNA(2,N)*RHON(2,N)),N=1,NFX)
      ELSEIF( IPNV.EQ.105 ) THEN
        WRITE(IPL,'(/,A)')'Trapped Gas Saturation '
        WRITE(IPL,FORM) (SGT(2,N),N=1,NFX)
      ELSEIF( IPNV.EQ.106 ) THEN
        WRITE(IPL,'(/,A)')'Trapped NAPL Saturation '
        WRITE(IPL,FORM) (SNT(2,N),N=1,NFX)
      ELSEIF( IPNV.EQ.107 ) THEN
        WRITE(IPL,'(/,A)')'Aqueous Trapped Gas Saturation '
        WRITE(IPL,FORM) (SGTL(N),N=1,NFX)
      ELSEIF( IPNV.EQ.108 ) THEN
        WRITE(IPL,'(/,A)')'NAPL Trapped Gas Saturation '
        WRITE(IPL,FORM) (SGTN(N),N=1,NFX)
      ELSEIF( IPNV.EQ.109 ) THEN
        INDX = 4
        IUNM = -3
        IUNKG = 1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        IF( IOM.GE.36 .AND. IOM.LE.39 ) THEN
          WRITE(IPL,'(/,2A)') 'Disolved-CH4 Aqueous Concentration, '
     &      ,UNPLOT(IPNV)
        ELSE
          WRITE(IPL,'(/,2A)') 'Disolved-Oil Aqueous Concentration, '
     &      ,UNPLOT(IPNV)
        ENDIF
        WRITE(IPL,FORM) (VAR*XLO(2,N)*RHOL(2,N),N=1,NFX)
      ELSEIF( IPNV.EQ.110 ) THEN
        WRITE(IPL,'(/,A)') 'Aqueous Salt Mass Fraction'
        WRITE(IPL,FORM) (XLS(2,N),N=1,NFX)
      ELSEIF( IPNV.EQ.111 ) THEN
        INDX = 4
        IUNM = -3
        IUNKG = 1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Surfactant Volumetric Concentration, '
     &    ,UNPLOT(IPNV)
        WRITE(IPL,FORM) (VAR*TMS(2,N),N=1,NFX)
      ELSEIF( IPNV.EQ.112 ) THEN
        INDX = 4
        IUNM = -3
        IUNKG = 1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Surfactant Aqueous Concentration, '
     &    ,UNPLOT(IPNV)
        WRITE(IPL,FORM) (VAR*XLS(2,N)*RHOL(2,N),N=1,NFX)
      ENDIF
!
!---  Deallocate memory  ---
!
      DEALLOCATE( DVAR,STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Deallocation Error: DVAR'
        CALL WRMSGS( INDX )
      ENDIF
      ISUB_LOG = ISUB_LOG-1
!
!---  End of WRPL_7 group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE WRPL_8( FORM,IPNV,IPNVGC,NFX )
!
!-------------------------Disclaimer-----------------------------------!
!
!     This material was prepared as an account of work sponsored by
!     an agency of the United States Government. Neither the
!     United States Government nor the United States Department of
!     Energy, nor Battelle, nor any of their employees, makes any
!     warranty, express or implied, or assumes any legal liability or
!     responsibility for the accuracy, completeness, or usefulness
!     of any information, apparatus, product, software or process
!     disclosed, or represents that its use would not infringe
!     privately owned rights.
!
!----------------------Acknowledgement---------------------------------!
!
!     This software and its documentation were produced with Government
!     support under Contract Number DE-AC06-76RLO-1830 awarded by the
!     United Department of Energy. The Government retains a paid-up
!     non-exclusive, irrevocable worldwide license to reproduce,
!     prepare derivative works, perform publicly and display publicly
!     by or for the Government, including the right to distribute to
!     other Government contractors.
!
!---------------------Copyright Notices--------------------------------!
!
!            Copyright Battelle Memorial Institute, 1996
!                    All Rights Reserved.
!
!----------------------Description-------------------------------------!
!
!     Write plot files for the following variables:
!
!     113 surfactant aqueous mass fraction 'XLS '
!           or gas N2 mass fraction 'XGN'
!     114 x surfactant flux ' US '
!     115 y surfactant flux ' VS '
!     116 z surfactant flux ' WS '
!     117 xnc surfactant flux 'USNC'
!     118 ync surfactant flux 'VSNC'
!     119 znc surfactant flux 'WSNC'
!     120 x dissolved-oil flux 'ULO '
!     121 y dissolved-oil flux 'VLO '
!     122 z dissolved-oil flux 'WLO '
!     123 xnc dissolved-oil flux 'ULOC'
!     124 ync dissolved-oil flux 'VLOC'
!     125 znc dissolved-oil flux 'WLOC'
!     126 NAPL-aqueous trapping number 'TPNL'
!     127 minimum effect aqueous saturation 'ESLM'
!     128 water vapor partial pressure 'PVW '
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 31 December 2003.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE OUTPU
      USE HYST
      USE GRID
      USE FLUXS
      USE FLUXD
      USE FILES
      USE FDVS
      USE FDVP
      USE FDVH
      USE FDVD
!

!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
      REAL*8, DIMENSION(:), ALLOCATABLE ::  DVAR
!
!----------------------Parameter Statements----------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*16 FORM
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/WRPL_8'
      IF( INDEX(SVN_ID(277)(1:1),'$').EQ.0 ) SVN_ID(277) =
     & '$Id: wrplot.F 1080 2017-03-14 16:22:02Z d3c002 $' 
!
!---  Dynamic memory allocation  ---
!
      ALLOCATE( DVAR(1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DVAR'
        CALL WRMSGS( INDX )
      ENDIF
      VAR = 1.D+0
      IPNVGCX = IPNVGC
      IF( IPNV.EQ.113 ) THEN
        IF( IOM.EQ.39 ) THEN
          WRITE(IPL,'(/,A)') 'Gas N2 Mass Fraction'
          WRITE(IPL,FORM) (XGN(2,N),N=1,NFX)
        ELSE
          WRITE(IPL,'(/,A)') 'Aqueous Surfactant Mass Fraction'
          WRITE(IPL,FORM) (XLS(2,N),N=1,NFX)
        ENDIF
      ELSEIF( IPNV.EQ.117 ) THEN
        INDX = 4
        IUNKG = 1
        IUNM = -2
        IUNS = -1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'X-Dir. Surfactant Flux (Node Centered) '
     &    ,UNPLOT(IPNV)
        DO N = 1,NFX
          IF( IXP(N).EQ.0 ) THEN
            DVAR(N) = 0.D+0
          ELSE
            DVAR(N) = VAR*0.5D+0*(US(1,NSX(N))+US(1,NSX(N)+1))
          ENDIF
        ENDDO
        WRITE(IPL,FORM) (DVAR(N),N=1,NFX)
      ELSEIF( IPNV.EQ.118 ) THEN
        INDX = 4
        IUNKG = 1
        IUNM = -2
        IUNS = -1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Y-Dir. Surfactant Flux (Node Centered) '
     &    ,UNPLOT(IPNV)
        DO N = 1,NFX
          IF( IXP(N).EQ.0 ) THEN
            DVAR(N) = 0.D+0
          ELSE
            DVAR(N) = VAR*0.5D+0*(VS(1,NSY(N))+VS(1,NSY(N)+IFLD))
          ENDIF
        ENDDO
        WRITE(IPL,FORM) (DVAR(N),N=1,NFX)
      ELSEIF( IPNV.EQ.119 ) THEN
        INDX = 4
        IUNKG = 1
        IUNM = -2
        IUNS = -1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Z-Dir. Surfactant Flux (Node Centered) '
     &    ,UNPLOT(IPNV)
        DO N = 1,NFX
          IF( IXP(N).EQ.0 ) THEN
            DVAR(N) = 0.D+0
          ELSE
            DVAR(N) = VAR*0.5D+0*(WS(1,NSZ(N))+WS(1,NSZ(N)+IJFLD))
          ENDIF
        ENDDO
        WRITE(IPL,FORM) (DVAR(N),N=1,NFX)
      ELSEIF( IPNV.EQ.123 ) THEN
        INDX = 4
        IUNKG = 1
        IUNM = -2
        IUNS = -1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'X-Dir. Dissolved-Oil Flux (Node Centered) '
     &    ,UNPLOT(IPNV)
        DO N = 1,NFX
          IF( IXP(N).EQ.0 ) THEN
            DVAR(N) = 0.D+0
          ELSE
            DVAR(N) = VAR*0.5D+0*(ULO(1,NSX(N))+ULO(1,NSX(N)+1))
          ENDIF
        ENDDO
        WRITE(IPL,FORM) (DVAR(N),N=1,NFX)
      ELSEIF( IPNV.EQ.124 ) THEN
        INDX = 4
        IUNKG = 1
        IUNM = -2
        IUNS = -1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Y-Dir. Dissolved-Oil Flux (Node Centered) '
     &    ,UNPLOT(IPNV)
        DO N = 1,NFX
          IF( IXP(N).EQ.0 ) THEN
            DVAR(N) = 0.D+0
          ELSE
            DVAR(N) = VAR*0.5D+0*(VLO(1,NSY(N))+VLO(1,NSY(N)+IFLD))
          ENDIF
        ENDDO
        WRITE(IPL,FORM) (DVAR(N),N=1,NFX)
      ELSEIF( IPNV.EQ.125 ) THEN
        INDX = 4
        IUNKG = 1
        IUNM = -2
        IUNS = -1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Z-Dir. Dissolved-Oil Flux (Node Centered) '
     &    ,UNPLOT(IPNV)
        DO N = 1,NFX
          IF( IXP(N).EQ.0 ) THEN
            DVAR(N) = 0.D+0
          ELSE
            DVAR(N) = VAR*0.5D+0*(WLO(1,NSZ(N))+WLO(1,NSZ(N)+IJFLD))
          ENDIF
        ENDDO
        WRITE(IPL,FORM) (DVAR(N),N=1,NFX)
      ELSEIF( IPNV.EQ.126 ) THEN
        WRITE(IPL,'(/,A)') 'NAPL-Aqueous Trapping Number'
        WRITE(IPL,FORM) (TRPNL(2,N),N=1,NFX)
      ELSEIF( IPNV.EQ.127 ) THEN
        WRITE(IPL,'(/,A)') 'Minimum Effective Aqueous Saturation'
        WRITE(IPL,FORM) (ASLMIN(2,N),N=1,NFX)
      ELSEIF( IPNV.EQ.128 ) THEN
        INDX = 4
        IUNM = -1
        IUNKG = 1
        IUNS = -2
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Water Vapor Partial Pressure, ',
     &    UNPLOT(IPNV)
        WRITE(IPL,FORM) (VAR*PVW(2,N),N=1,NFX)
      ENDIF
!
!---  Deallocate memory  ---
!
      DEALLOCATE( DVAR,STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Deallocation Error: DVAR'
        CALL WRMSGS( INDX )
      ENDIF
      ISUB_LOG = ISUB_LOG-1
!
!---  End of WRPL_8 group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE WRPL_9( FORM,IPNV,IPNVGC,NFX )
!
!-------------------------Disclaimer-----------------------------------!
!
!     This material was prepared as an account of work sponsored by
!     an agency of the United States Government. Neither the
!     United States Government nor the United States Department of
!     Energy, nor Battelle, nor any of their employees, makes any
!     warranty, express or implied, or assumes any legal liability or
!     responsibility for the accuracy, completeness, or usefulness
!     of any information, apparatus, product, software or process
!     disclosed, or represents that its use would not infringe
!     privately owned rights.
!
!----------------------Acknowledgement---------------------------------!
!
!     This software and its documentation were produced with Government
!     support under Contract Number DE-AC06-76RLO-1830 awarded by the
!     United Department of Energy. The Government retains a paid-up
!     non-exclusive, irrevocable worldwide license to reproduce,
!     prepare derivative works, perform publicly and display publicly
!     by or for the Government, including the right to distribute to
!     other Government contractors.
!
!---------------------Copyright Notices--------------------------------!
!
!            Copyright Battelle Memorial Institute, 1996
!                    All Rights Reserved.
!
!----------------------Description-------------------------------------!
!
!     Write plot files for the following variables:
!
!     129 air partial pressure 'PVA '
!     130 oil vapor partial pressure 'PVO '
!     130 stress-zz, Pa
!     131 gas-aqueous scaling factor 'BGL '
!     132 free-NAPL aqueous interfacial area 'ANFL'
!     133 trapped-NAPL aqueous interfacial area 'ANTL'
!     134 aqueous solute coefficient 'HKL '
!     135 free-NAPL solute coefficient 'HKNF'
!     136 trapped-NAPL solute coefficient 'HKNT'
!     137 undefined
!     138 undefined
!     139 volumetric molar density 'RHMV'
!     140 water mass source rate 'SRCW'
!     141 air mass source rate 'SRCA'
!     142 oil mass source rate 'SRCO'
!     143 energy source rate 'SRCQ'
!     144 aqueous well depth 'PLWB'
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 31 December 2003.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOURC
      USE SOLTN
      USE OUTPU
      USE GRID
      USE GEOMECH
      USE FILES
      USE FDVP
      USE FDVN
      USE FDVGC
!

!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Parameter Statements----------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*16 FORM
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/WRPL_9'
      IF( INDEX(SVN_ID(277)(1:1),'$').EQ.0 ) SVN_ID(277) =
     & '$Id: wrplot.F 1080 2017-03-14 16:22:02Z d3c002 $' 
      VAR = 1.D+0
      IF( IPNV.EQ.129 ) THEN
        IF( IOM.EQ.34 ) THEN
          WRITE(IPL,'(/,A)') 'Saturated Dissolved-CO2 Mass Fraction'
          WRITE(IPL,FORM) (PVO(2,N),N=1,NFX)
        ELSEIF( IOM.GE.32 .AND. IOM.LE.33 ) THEN
          INDX = 4
          IUNM = -1
          IUNKG = 1
          IUNS = -2
          CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
          WRITE(IPL,'(/,2A)') 'CO2 Partial Presure, ',
     &      UNPLOT(IPNV)
          WRITE(IPL,FORM) (VAR*PVA(2,N),N=1,NFX)
        ELSEIF( IOM.EQ.30 .OR. IOM.EQ.40 .OR. IOM.EQ.43 ) THEN
          INDX = 4
          IUNM = -1
          IUNKG = 1
          IUNS = -2
          CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
          NCHX = INDEX(GCNM(IPNVGC)(1:),'  ')-1
          WRITE(IPL,'(/,2A)') GCNM(IPNVGC)(1:NCHX) 
     &      // ' Partial Pressure, ',UNPLOT(IPNV)
          WRITE(IPL,FORM) (VAR*PVC(IPNVGC,2,N),N=1,NFX)
        ELSE
          INDX = 4
          IUNM = -1
          IUNKG = 1
          IUNS = -2
          CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
          WRITE(IPL,'(/,2A)') 'Air Partial Presure, ',
     &      UNPLOT(IPNV)
          WRITE(IPL,FORM) (VAR*PVA(2,N),N=1,NFX)
        ENDIF
      ELSEIF( IPNV.EQ.130 ) THEN
        INDX = 4
        IUNM = -1
        IUNKG = 1
        IUNS = -2
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
!
!---    Geomechanics  ---
!
        IF( ISLC(50).NE.0 ) THEN
          WRITE(IPL,'(/,2A)') 'Stress-zz, ',UNPLOT(IPNV)
          WRITE(IPL,FORM) (VAR*SIG_GM(3,N),N=1,NFX)
        ELSE
          WRITE(IPL,'(/,2A)')'Oil Vapor Partial Pressure ',UNPLOT(IPNV)
          WRITE(IPL,FORM) (VAR*PVA(2,N),N=1,NFX)
        ENDIF
      ELSEIF( IPNV.EQ.131 ) THEN
        WRITE(IPL,'(/,A)') 'Gas-Aqueous Surf. Tension Scaling Factor'
        WRITE(IPL,FORM) (BTGL(2,N),N=1,NFX)
      ELSEIF( IPNV.EQ.132 ) THEN
        INDX = 4
        IUNM = -1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,3A)') 'Specific Aqueous-Free NAPL Interfacial ',
     &    'Area, ', UNPLOT(IPNV)
        WRITE(IPL,FORM) (VAR*ANLF(N),N=1,NFX)
      ELSEIF( IPNV.EQ.133 ) THEN
        INDX = 4
        IUNM = -1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,3A)') 'Specific Aqueous-Trapped NAPL ',
     &    'Interfacial Area, ', UNPLOT(IPNV)
        WRITE(IPL,FORM) (VAR*ANLT(N),N=1,NFX)
      ELSEIF( IPNV.EQ.134 ) THEN
        INDX = 4
        IUNS = -1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Aqueous Solute Coeff., ',
     &    UNPLOT(IPNV)
        WRITE(IPL,FORM) (VAR*HKL(N),N=1,NFX)
      ELSEIF( IPNV.EQ.135 ) THEN
        INDX = 4
        IUNS = -1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Free NAPL Solute Coeff., ',
     &    UNPLOT(IPNV)
        WRITE(IPL,FORM) (VAR*HKNF(N),N=1,NFX)
      ELSEIF( IPNV.EQ.136 ) THEN
        INDX = 4
        IUNS = -1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Trapped NAPL Solute Coeff., ',
     &    UNPLOT(IPNV)
        WRITE(IPL,FORM) (VAR*HKNT(N),N=1,NFX)
      ELSEIF( IPNV.EQ.137 ) THEN
        WRITE(IPL,'(/,A)')'Water Relative Humidity '
        WRITE(IPL,FORM) ((PVW(2,N)/PSW(2,N)),N=1,NFX)
      ELSEIF( IPNV.EQ.140 ) THEN
        INDX = 4
        IUNKG = 1
        IUNS = -1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Water Mass Source Rate, ',
     &    UNPLOT(IPNV)
        WRITE(IPL,FORM) (VAR*SRCW(2,N),N=1,NFX)
      ELSEIF( IPNV.EQ.141 ) THEN
        INDX = 4
        IUNKG = 1
        IUNS = -1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        IF( IOM.GE.32 .AND. IOM.LE.39 ) THEN
          WRITE(IPL,'(/,2A)') 'CO2 Mass Source Rate, ',
     &      UNPLOT(IPNV)
        ELSE
          WRITE(IPL,'(/,2A)') 'Air Mass Source Rate, ',
     &      UNPLOT(IPNV)
        ENDIF
        WRITE(IPL,FORM) (VAR*SRCA(2,N),N=1,NFX)
      ELSEIF( IPNV.EQ.142 ) THEN
        INDX = 4
        IUNKG = 1
        IUNS = -1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Oil Mass Source Rate, ',
     &    UNPLOT(IPNV)
        WRITE(IPL,FORM) (VAR*SRCO(2,N),N=1,NFX)
      ELSEIF( IPNV.EQ.143 ) THEN
        INDX = 4
        IUNM = 2
        IUNKG = 1
        IUNS = -3
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Thermal Energy Source Rate, ',
     &    UNPLOT(IPNV)
        WRITE(IPL,FORM) (VAR*SRCT(2,N),N=1,NFX)
      ENDIF
      ISUB_LOG = ISUB_LOG-1
!
!---  End of WRPL_9 group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE WRPL_10( FORM,IPNV,IPNVGC,NFX )
!
!-------------------------Disclaimer-----------------------------------!
!
!     This material was prepared as an account of work sponsored by
!     an agency of the United States Government. Neither the
!     United States Government nor the United States Department of
!     Energy, nor Battelle, nor any of their employees, makes any
!     warranty, express or implied, or assumes any legal liability or
!     responsibility for the accuracy, completeness, or usefulness
!     of any information, apparatus, product, software or process
!     disclosed, or represents that its use would not infringe
!     privately owned rights.
!
!----------------------Acknowledgement---------------------------------!
!
!     This software and its documentation were produced with Government
!     support under Contract Number DE-AC06-76RLO-1830 awarded by the
!     United Department of Energy. The Government retains a paid-up
!     non-exclusive, irrevocable worldwide license to reproduce,
!     prepare derivative works, perform publicly and display publicly
!     by or for the Government, including the right to distribute to
!     other Government contractors.
!
!---------------------Copyright Notices--------------------------------!
!
!            Copyright Battelle Memorial Institute, 1996
!                    All Rights Reserved.
!
!----------------------Description-------------------------------------!
!
!     Write plot files for the following variables:
!
!     145 well flow rate 'QLW '
!     146 well flow integral 'QLWI'
!     147 salt mass source rate 'SRCS'
!     148 salt mass source integral 'SRIS'
!     149 scanning path 'PATH'
!     150 aqueous air or co2 saturation 'DAPS'
!           or Webb matching point saturation 'WMPS'
!     151 bubble void fraction 'BVF '
!           or aqueous N2 mole fraction 'XMLN'
!     152 bubble air mass fraction 'XBA '
!           or aqueous N2 mass fraction 'XLN'
!     153 mineralized co2 'MCO2 '
!     154 napl well flow rate 'QNW '
!     155 napl well flow integral 'QNWI'
!     156 total well flow rate 'QTW '
!     157 total well flow integral 'QTWI'
!     160 gas N2 mole fraction 'XMGN'
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 31 December 2003.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOURC
      USE SOLTN
      USE PORMED
      USE OUTPU
      USE HYST
      USE GRID
      USE FILES
      USE FDVP
      USE FDVH
      USE CONST
!

!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Parameter Statements----------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*16 FORM
      REAL*8, DIMENSION(:), ALLOCATABLE ::  DVAR
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/WRPL_10'
      IF( INDEX(SVN_ID(277)(1:1),'$').EQ.0 ) SVN_ID(277) =
     & '$Id: wrplot.F 1080 2017-03-14 16:22:02Z d3c002 $' 
!
!---  Dynamic memory allocation  ---
!
      ALLOCATE( DVAR(1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DVAR'
        CALL WRMSGS( INDX )
      ENDIF
      VAR = 1.D+0
      IPNVGCX = IPNVGC
      IF( IPNV.EQ.147 ) THEN
        INDX = 4
        IUNKG = 1
        IUNS = -1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Salt Mass Source Rate, ',
     &    UNPLOT(IPNV)
        WRITE(IPL,FORM) (VAR*SRCS(2,N),N=1,NFX)
      ELSEIF( IPNV.EQ.148 ) THEN
        INDX = 4
        IUNKG = 1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Salt Mass Source Integral, ',
     &    UNPLOT(IPNV)
        WRITE(IPL,FORM) (VAR*SRCIS(N),N=1,NFX)
      ELSEIF( IPNV.EQ.149 ) THEN
        WRITE(IPL,'(/,A)') 'Scanning Path (-1=Drying, 1=Wetting), '
        DO 149 N = 1,NFX
          DVAR(N) = REAL(IPH(2,N))
          IF( IXP(N).EQ.0 ) DVAR(N) = 0.D+0
  149   CONTINUE
        WRITE(IPL,FORM) (DVAR(N),N=1,NFX)
      ELSEIF( IPNV.EQ.150 ) THEN
        IF( IOM.EQ.32 .OR. IOM.EQ.33 ) THEN
          WRITE(IPL,'(/,A)')'Webb Matching Point Saturation '
          WRITE(IPL,FORM) (SCHR(8,IZ(N)),N=1,NFX)
        ELSE
          WRITE(IPL,'(/,A)')'Dissolved Air Percent Saturation '
          WRITE(IPL,FORM) ((XMLA(2,N)*HCAW/(PVA(2,N)+SMALL)),N=1,NFX)
        ENDIF
      ELSEIF( IPNV.EQ.151 ) THEN
        IF( IOM.EQ.39 ) THEN
          WRITE(IPL,'(/,A)')'Aqueous N2 Mole Fraction'
          WRITE(IPL,FORM) (XMLN(2,N),N=1,NFX)
        ELSE
          WRITE(IPL,'(/,A)')'Bubble Void Fraction '
          WRITE(IPL,FORM) (SN(2,N),N=1,NFX)
        ENDIF
      ELSEIF( IPNV.EQ.152 ) THEN
        IF( IOM.EQ.39 ) THEN
          WRITE(IPL,'(/,A)')'Aqueous N2 Mass Fraction'
          WRITE(IPL,FORM) (XLN(2,N),N=1,NFX)
        ELSE
          WRITE(IPL,'(/,A)')'Bubble Air Concentration '
          INDX = 4
          IUNKG = 1
          IUNM = -3
          CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
          WRITE(IPL,FORM) (VAR*XGO(2,N),N=1,NFX)
        ENDIF
      ELSEIF( IPNV.EQ.153 ) THEN
        WRITE(IPL,'(/,A)')'Mineralized CO2 Concentration '
        INDX = 4
        IUNKG = 1
        IUNM = -3
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,FORM) (VAR*XLO(2,N),N=1,NFX)
      ELSEIF( IPNV.EQ.160 ) THEN
        IF( IOM.EQ.39 ) THEN
          WRITE(IPL,'(/,A)')'Gas N2 Mole Fraction '
          WRITE(IPL,FORM) (XMGN(2,N),N=1,NFX)
        ENDIF
      ENDIF
!
!---  Deallocate memory  ---
!
      DEALLOCATE( DVAR,STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Deallocation Error: DVAR'
        CALL WRMSGS( INDX )
      ENDIF
      ISUB_LOG = ISUB_LOG-1
!
!---  End of WRPL_10 group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE WRPL_11( FORM,IPNV,IPNVGC,NFX )
!
!-------------------------Disclaimer-----------------------------------!
!
!     This material was prepared as an account of work sponsored by
!     an agency of the United States Government. Neither the
!     United States Government nor the United States Department of
!     Energy, nor Battelle, nor any of their employees, makes any
!     warranty, express or implied, or assumes any legal liability or
!     responsibility for the accuracy, completeness, or usefulness
!     of any information, apparatus, product, software or process
!     disclosed, or represents that its use would not infringe
!     privately owned rights.
!
!----------------------Acknowledgement---------------------------------!
!
!     This software and its documentation were produced with Government
!     support under Contract Number DE-AC06-76RLO-1830 awarded by the
!     United Department of Energy. The Government retains a paid-up
!     non-exclusive, irrevocable worldwide license to reproduce,
!     prepare derivative works, perform publicly and display publicly
!     by or for the Government, including the right to distribute to
!     other Government contractors.
!
!---------------------Copyright Notices--------------------------------!
!
!            Copyright Battelle Memorial Institute, 1996
!                    All Rights Reserved.
!
!----------------------Description-------------------------------------!
!
!     Write plot files for the following variables:
!
!     161 NAPL dissolved water concentration 'CNW '
!     162 NAPL dissolved water mole fraction 'XMNW'
!           or N2 nonaqueous liquid mole fraction 'XMNN'
!     163 NAPL dissolved water mass fraction 'XNW '
!           or H2O liquid-CO2 mass fraction 'XNW '
!     164 NAPL dissolved oil concentration 'CNO '
!     165 NAPL dissolved oil mole fraction 'XMNO'
!           or CH4 nonaqueous liquid mole fraction 'XMNO'
!     166 NAPL dissolved oil mass fraction 'XNO '
!           or CH4 liquid-CO2 mass fraction 'XNO '
!     167 total alcohol mass 'TMA '
!     168 aqueous dissolved water mass fraction 'XLW '
!     169 alcohol mass source integral 'SRIA'
!     170 alcohol mass source rate 'SRCA'
!     171 integrated NAPL and aqueous dissolved alcohol 'IMA '
!     172 integrated aqueous dissolved alcohol 'IMLA'
!     173 integrated NAPL dissolved water 'IMNW'
!     174 integrated NAPL dissolved oil 'IMNO'
!     175 integrated NAPL dissolved alcohol 'IMNA'
!     176 aqueous viscosity 'VISL'
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 31 December 2003.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOURC
      USE SOLTN
      USE OUTPU
      USE GRID
      USE FILES
      USE FDVP
      USE FDVA
!

!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Parameter Statements----------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      REAL*8, DIMENSION(:), ALLOCATABLE ::  DVAR
      CHARACTER*16 FORM
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/WRPL_11'
      IF( INDEX(SVN_ID(277)(1:1),'$').EQ.0 ) SVN_ID(277) =
     & '$Id: wrplot.F 1080 2017-03-14 16:22:02Z d3c002 $' 
!
!---  Dynamic memory allocation  ---
!
      ALLOCATE( DVAR(1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DVAR'
        CALL WRMSGS( INDX )
      ENDIF
      VAR = 1.D+0
      IPNVGCX = IPNVGC
      IF( IPNV.EQ.161 ) THEN
        WRITE(IPL,'(/,A)')'NAPL Water Concentration '
        WRITE(IPL,FORM) ((XNW(2,N)*RHON(2,N)),N=1,NFX)
      ELSEIF( IPNV.EQ.162 ) THEN
        IF( IOM.EQ.39 ) THEN
          WRITE(IPL,'(/,A)')'Nonaqueous Liquid N2 Mole Fraction '
          WRITE(IPL,FORM) (XMNN(2,N),N=1,NFX)
        ELSE
          WRITE(IPL,'(/,A)')'NAPL Water Mole Fraction '
          WRITE(IPL,FORM) (XMNW(2,N),N=1,NFX)
        ENDIF
      ELSEIF( IPNV.EQ.163 ) THEN
        IF( IOM.GE.36 .AND. IOM.LE.38 ) THEN
          WRITE(IPL,'(/,A)')'Liquid-CO2 H2O Mass Fraction '
        ELSEIF( IOM.EQ.39 ) THEN
          WRITE(IPL,'(/,A)')'Nonaqueous Liquid H2O Mass Fraction '
        ELSE
          WRITE(IPL,'(/,A)')'NAPL Water Mass Fraction '
        ENDIF
        WRITE(IPL,FORM) (XNW(2,N),N=1,NFX)
      ELSEIF( IPNV.EQ.164 ) THEN
        WRITE(IPL,'(/,A)')'NAPL Oil Concentration '
        WRITE(IPL,FORM) ((XNO(2,N)*RHON(2,N)),N=1,NFX)
      ELSEIF( IPNV.EQ.165 ) THEN
        IF( IOM.EQ.39 ) THEN
          WRITE(IPL,'(/,A)')'Nonaqueous Liquid CH4 Mole Fraction '
          WRITE(IPL,FORM) (XMNO(2,N),N=1,NFX)
        ELSE
          WRITE(IPL,'(/,A)')'NAPL Oil Mole Fraction '
          WRITE(IPL,FORM) (XMNO(2,N),N=1,NFX)
        ENDIF
      ELSEIF( IPNV.EQ.166 ) THEN
        IF( IOM.GE.36 .AND. IOM.LE.38 ) THEN
          WRITE(IPL,'(/,A)')'Liquid-CO2 CH4 Mass Fraction '
        ELSEIF( IOM.GE.39 ) THEN
          WRITE(IPL,'(/,A)')'Nonaqeuous Liquid CH4 Mass Fraction '
        ELSE
          WRITE(IPL,'(/,A)')'NAPL Oil Mass Fraction '
        ENDIF
        WRITE(IPL,FORM) (XNO(2,N),N=1,NFX)
      ELSEIF( IPNV.EQ.167 ) THEN
        INDX = 4
        IUNKG = 1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        DO 167 N = 1,NFX
          DVAR(N) = 0.D+0
          DVAR(N) = DVAR(N) +
     &      VAR*PORD(2,N)*VOL(N)*XLA(2,N)*SL(2,N)*RHOL(2,N)
          IF( INAPL.EQ.1 ) DVAR(N) = DVAR(N) +
     &      VAR*PORD(2,N)*VOL(N)*XNA(2,N)*SN(2,N)*RHON(2,N)
  167   CONTINUE
        WRITE(IPL,'(/,2A)') 'Total Alcohol Mass, ',UNPLOT(IPNV)
        WRITE(IPL,FORM) (DVAR(N),N=1,NFX)
      ELSEIF( IPNV.EQ.168 ) THEN
        WRITE(IPL,'(/,A)') 'Aqueous Water Mole Fraction'
        WRITE(IPL,FORM) (XMLW(2,N),N=1,NFX)
      ELSEIF( IPNV.EQ.169 ) THEN
        INDX = 4
        IUNKG = 1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Alcohol Mass Source Integral, ',
     &    UNPLOT(IPNV)
        WRITE(IPL,FORM) (VAR*SRCIA(N),N=1,NFX)
      ELSEIF( IPNV.EQ.170 ) THEN
        INDX = 4
        IUNKG = 1
        IUNS = -1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Alcohol Mass Source Rate, ',
     &    UNPLOT(IPNV)
        WRITE(IPL,FORM) (VAR*SRCA(2,N),N=1,NFX)
!
!---  Aqueous viscosity  ---
!
      ELSEIF( IPNV.EQ.176 ) THEN
        INDX = 4
        IUNM = -1
        IUNKG = 1
        IUNS = -1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Aqueous Viscosity, ',UNPLOT(IPNV)
        WRITE(IPL,FORM) (VAR*VISL(2,N),N=1,NFX)
      ENDIF
!
!---  Deallocate memory  ---
!
      DEALLOCATE( DVAR,STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Deallocation Error: DVAR'
        CALL WRMSGS( INDX )
      ENDIF
      ISUB_LOG = ISUB_LOG-1
!
!---  End of WRPL_11 group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE WRPL_12( FORM,IPNV,IPNVGC,NFX )
!
!-------------------------Disclaimer-----------------------------------!
!
!     This material was prepared as an account of work sponsored by
!     an agency of the United States Government. Neither the
!     United States Government nor the United States Department of
!     Energy, nor Battelle, nor any of their employees, makes any
!     warranty, express or implied, or assumes any legal liability or
!     responsibility for the accuracy, completeness, or usefulness
!     of any information, apparatus, product, software or process
!     disclosed, or represents that its use would not infringe
!     privately owned rights.
!
!----------------------Acknowledgement---------------------------------!
!
!     This software and its documentation were produced with Government
!     support under Contract Number DE-AC06-76RLO-1830 awarded by the
!     United Department of Energy. The Government retains a paid-up
!     non-exclusive, irrevocable worldwide license to reproduce,
!     prepare derivative works, perform publicly and display publicly
!     by or for the Government, including the right to distribute to
!     other Government contractors.
!
!---------------------Copyright Notices--------------------------------!
!
!            Copyright Battelle Memorial Institute, 1996
!                    All Rights Reserved.
!
!----------------------Description-------------------------------------!
!
!     Write plot files for the following variables:
!
!     177 monitoring well water depth or total-liquid well depth 'WDT '
!     178 aqueous well depth 'WDL '
!     179 NAPL well depth 'WDN '
!     180 monitoring well pressure ' PW '
!     181 monitoring well aqueous saturation 'SLW '
!     182 monitoring well water-vapor mass fraction or well NAPL saturation 'XGWW'
!     183 monitoring well dissolved-air mass fraction or dissolved-oil mass fraction 'XLAW'
!     184 monitoring well axial aqueous flux or well total-liquid pumping rate 'UL_W'
!     185 monitoring well axial gas flux or well aqueous pumping rate 'UG_W'
!     186 monitoring well vertical aqueous flux or well NAPL pumping rate 'WL_W'
!     187 monitoring well vertical gas flux or well total-liquid pumping integral 'WG_W'
!     188 integrated well aqueous pumping 'IPLW'
!     189 integrated mineral CO2 or well NAPL pumping integral 'IPNW'
!     190 integrated trapped gas air 'IMGT'
!     191 integrated water mass 'IMW '
!     192 integrated air mass 'IMA '
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 31 December 2003.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE WELL_FX
      USE WELL_FD
      USE WELL_CL
      USE SOLTN
      USE OUTPU
      USE GRID
      USE FILES
      USE FDVP
      USE CONST
!

!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Parameter Statements----------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      REAL*8, DIMENSION(:), ALLOCATABLE ::  DVAR
      CHARACTER*16 FORM
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/WRPL_12'
      IF( INDEX(SVN_ID(277)(1:1),'$').EQ.0 ) SVN_ID(277) =
     & '$Id: wrplot.F 1080 2017-03-14 16:22:02Z d3c002 $' 
!
!---  Dynamic memory allocation  ---
!
      ALLOCATE( DVAR(1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DVAR'
        CALL WRMSGS( INDX )
      ENDIF
      VAR = 1.D+0
      IPNVGCX = IPNVGC
      IF( IPNV.EQ.178 ) THEN
        INDX = 4
        IUNM = 1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Matric Potential Head, ',UNPLOT(IPNV)
        WRITE(IPL,FORM) (VAR*(PL(2,N)-PG(2,N))/RHORL/GRAV,N=1,NFX)
      ELSEIF( IPNV.EQ.180 ) THEN
        DO 180 N = 1,NFX
          NW = ABS(IXW(N))
          IF( NW.EQ.0 ) THEN
            DVAR(N) =  0.D+0
          ELSE
            DVAR(N) = MAX(PWLW(2,NW),PWGW(2,NW)) + PATM
          ENDIF
  180   CONTINUE
        INDX = 4
        IUNM = -1
        IUNKG = 1
        IUNS = -2
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Monitoring Well Pressure, ',
     &    UNPLOT(IPNV)
        WRITE(IPL,FORM) ((VAR*DVAR(N)),N=1,NFX)
      ELSEIF( IPNV.EQ.181 ) THEN
        DO 181 N = 1,NFX
          NW = ABS(IXW(N))
          IF( NW.EQ.0 ) THEN
            DVAR(N) =  0.D+0
          ELSE
            DVAR(N) = SLW(2,NW)
          ENDIF
  181   CONTINUE
        WRITE(IPL,'(/,A)') 'Monitoring Well Aqueous Saturation'
        WRITE(IPL,FORM) (DVAR(N),N=1,NFX)
      ELSEIF( IPNV.EQ.182 ) THEN
        DO 182 N = 1,NFX
          NW = ABS(IXW(N))
          IF( NW.EQ.0 ) THEN
            DVAR(N) =  0.D+0
          ELSE
            DVAR(N) = XGWW(2,NW)
          ENDIF
  182   CONTINUE
        WRITE(IPL,'(/,A)') 'Monitoring Well Water-Vapor Mass Fraction'
        WRITE(IPL,FORM) (DVAR(N),N=1,NFX)
      ELSEIF( IPNV.EQ.183 ) THEN
        DO 183 N = 1,NFX
          NW = ABS(IXW(N))
          IF( NW.EQ.0 ) THEN
            DVAR(N) =  0.D+0
          ELSE
            DVAR(N) = XLAW(2,NW)
          ENDIF
  183   CONTINUE
        IF( IOM.GE.32 .AND. IOM.LE.34 ) THEN
          WRITE(IPL,'(/,A)') 'Monitoring Well Dissolved-CO2 ' //
     &     'Mass Fraction'
        ELSE
          WRITE(IPL,'(/,A)') 'Monitoring Well Dissolved-Air ' //
     &      'Mass Fraction'
        ENDIF
        WRITE(IPL,FORM) (DVAR(N),N=1,NFX)
      ELSEIF( IPNV.EQ.184 ) THEN
        DO 184 N = 1,NFX
          NW = ABS(IXW(N))
          IF( NW.EQ.0 ) THEN
            DVAR(N) =  0.D+0
          ELSE
            DVAR(N) = UL_W(1,NW)
          ENDIF
  184   CONTINUE
        INDX = 4
        IUNM = 1
        IUNS = -1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Axial Aqueous Well Flux, ',
     &    UNPLOT(IPNV)
        WRITE(IPL,FORM) ((VAR*DVAR(N)),N=1,NFX)
      ELSEIF( IPNV.EQ.185 ) THEN
        DO 185 N = 1,NFX
          NW = ABS(IXW(N))
          IF( NW.EQ.0 ) THEN
            DVAR(N) =  0.D+0
          ELSE
            DVAR(N) = UG_W(1,NW)
          ENDIF
  185   CONTINUE
        INDX = 4
        IUNM = 1
        IUNS = -1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Axial Gas Well Flux, ',
     &    UNPLOT(IPNV)
        WRITE(IPL,FORM) ((VAR*DVAR(N)),N=1,NFX)
      ELSEIF( IPNV.EQ.186 ) THEN
        DO 186 N = 1,NFX
          NW = ABS(IXW(N))
          IF( NW.EQ.0 ) THEN
            DVAR(N) =  0.D+0
          ELSE
            DVAR(N) = WL_W(1,NW)
          ENDIF
  186   CONTINUE
        INDX = 4
        IUNM = 1
        IUNS = -1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Vertical Aqueous Well Flux, ',
     &    UNPLOT(IPNV)
        WRITE(IPL,FORM) ((VAR*DVAR(N)),N=1,NFX)
      ELSEIF( IPNV.EQ.187 ) THEN
        DO 187 N = 1,NFX
          NW = ABS(IXW(N))
          IF( NW.EQ.0 ) THEN
            DVAR(N) =  0.D+0
          ELSE
            DVAR(N) = WG_W(1,NW)
          ENDIF
  187   CONTINUE
        INDX = 4
        IUNM = 1
        IUNS = -1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Vertical Gas Well Flux, ',
     &    UNPLOT(IPNV)
        WRITE(IPL,FORM) ((VAR*DVAR(N)),N=1,NFX)
      ENDIF
!
!---  Deallocate memory  ---
!
      DEALLOCATE( DVAR,STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Deallocation Error: DVAR'
        CALL WRMSGS( INDX )
      ENDIF
      ISUB_LOG = ISUB_LOG-1
!
!---  End of WRPL_12 group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE WRPL_13( FORM,IPNV,IPNVGC,NFX )
!
!-------------------------Disclaimer-----------------------------------!
!
!     This material was prepared as an account of work sponsored by
!     an agency of the United States Government. Neither the
!     United States Government nor the United States Department of
!     Energy, nor Battelle, nor any of their employees, makes any
!     warranty, express or implied, or assumes any legal liability or
!     responsibility for the accuracy, completeness, or usefulness
!     of any information, apparatus, product, software or process
!     disclosed, or represents that its use would not infringe
!     privately owned rights.
!
!----------------------Acknowledgement---------------------------------!
!
!     This software and its documentation were produced with Government
!     support under Contract Number DE-AC06-76RLO-1830 awarded by the
!     United Department of Energy. The Government retains a paid-up
!     non-exclusive, irrevocable worldwide license to reproduce,
!     prepare derivative works, perform publicly and display publicly
!     by or for the Government, including the right to distribute to
!     other Government contractors.
!
!---------------------Copyright Notices--------------------------------!
!
!            Copyright Battelle Memorial Institute, 1996
!                    All Rights Reserved.
!
!----------------------Description-------------------------------------!
!
!     Write plot files for the following variables:
!
!     193 integrated oil mass 'IMO '
!     194 integrated aqueous water 'IMLW'
!     195 integrated aqueous air 'IMLA'
!     196 integrated aqueous oil 'IMLO'
!     197 integrated gas water 'IMGW'
!     198 integrated gas air 'IMGA'
!     199 integrated gas oil 'IMGO'
!     200 reserved to control plot file output
!     201 x aqueous relative permeability 'RKLX'
!     202 y aqueous relative permeability 'RKLY'
!     203 z aqueous relative permeability 'RKLZ'
!     204 aqueous co2 mole fraction 'XMLA'
!           or aqueous component mole fraction 'XMLC'
!     205   aqueous salt mole fraction 'XMLS'
!     206   atmospheric temperature ' TA '
!           or critical temperature ' TCR '
!     207   atmospheric relative humidity ' RH '
!     208   atmospheric solar radiation ' RN '
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 31 December 2003.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE OUTPU
      USE GRID
      USE FILES
      USE FDVS
      USE FDVP
      USE FDVH
      USE FDVGC
!

!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Parameter Statements----------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*16 FORM
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/WRPL_13'
      IF( INDEX(SVN_ID(277)(1:1),'$').EQ.0 ) SVN_ID(277) =
     & '$Id: wrplot.F 1080 2017-03-14 16:22:02Z d3c002 $' 
      VAR = 1.D+0
      IPNVGCX = IPNVGC
      IF( IPNV.EQ.201 ) THEN
        WRITE(IPL,'(/,A)') 'X-Aqueous Relative Permeability'
        WRITE(IPL,FORM) (RKL(1,2,N),N=1,NFX)
      ELSEIF ( IPNV.EQ.202 ) THEN
        WRITE(IPL,'(/,A)') 'Y-Aqueous Relative Permeability'
        WRITE(IPL,FORM) (RKL(2,2,N),N=1,NFX)
      ELSEIF ( IPNV.EQ.203 ) THEN
        WRITE(IPL,'(/,A)') 'Z-Aqueous Relative Permeability'
        WRITE(IPL,FORM) (RKL(3,2,N),N=1,NFX)
      ELSEIF ( IPNV.EQ.204 ) THEN
        IF( IOM.EQ.30 .OR. IOM.EQ.40 .OR. IOM.EQ.43 ) THEN
          NCHX = INDEX(GCNM(IPNVGC)(1:),'  ')-1
          WRITE(IPL,'(/,A)') 'Aqueous ' // GCNM(IPNVGC)(1:NCHX)
     &      // ' Mole Fraction'
          WRITE(IPL,FORM) (XMLC(IPNVGC,2,N),N=1,NFX)
        ELSE
        WRITE(IPL,'(/,A)') 'CO2 Aqueous Mole Fraction'
        WRITE(IPL,FORM) (XMLA(2,N),N=1,NFX)
        ENDIF
      ELSEIF ( IPNV.EQ.205 ) THEN
        WRITE(IPL,'(/,A)') 'Salt Aqueous Mole Fraction'
        WRITE(IPL,FORM) (XMLS(2,N),N=1,NFX)
      ELSEIF ( IPNV.EQ.206 ) THEN
        WRITE(IPL,'(/,2A)') 'Critical Temperature, ',UNPLOT(IPNV)
        IF( UNPLOT(IPNV).EQ.'c' ) THEN
          WRITE(IPL,FORM) (TCR(N),N=1,NFX)
        ELSEIF( UNPLOT(IPNV).EQ.'k' ) THEN
          WRITE(IPL,FORM) ((TCR(N)+273.15D+0),N=1,NFX)
        ELSEIF( UNPLOT(IPNV).EQ.'f' ) THEN
          WRITE(IPL,FORM) ((TCR(N)*1.8D+0+3.2D+1),N=1,NFX)
        ELSEIF( UNPLOT(IPNV).EQ.'r' ) THEN
          WRITE(IPL,FORM) ((TCR(N)*1.8D+0+4.92D+2),N=1,NFX)
        ENDIF
      ENDIF
      ISUB_LOG = ISUB_LOG-1
!
!---  End of WRPL_13 group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE WRPL_14( FORM,IPNV,IPNVGC,NFX )
!
!-------------------------Disclaimer-----------------------------------!
!
!     This material was prepared as an account of work sponsored by
!     an agency of the United States Government. Neither the
!     United States Government nor the United States Department of
!     Energy, nor Battelle, nor any of their employees, makes any
!     warranty, express or implied, or assumes any legal liability or
!     responsibility for the accuracy, completeness, or usefulness
!     of any information, apparatus, product, software or process
!     disclosed, or represents that its use would not infringe
!     privately owned rights.
!
!----------------------Acknowledgement---------------------------------!
!
!     This software and its documentation were produced with Government
!     support under Contract Number DE-AC06-76RLO-1830 awarded by the
!     United Department of Energy. The Government retains a paid-up
!     non-exclusive, irrevocable worldwide license to reproduce,
!     prepare derivative works, perform publicly and display publicly
!     by or for the Government, including the right to distribute to
!     other Government contractors.
!
!---------------------Copyright Notices--------------------------------!
!
!            Copyright Battelle Memorial Institute, 1996
!                    All Rights Reserved.
!
!----------------------Description-------------------------------------!
!
!     Write plot files for the following variables:
!
!     209 atmospheric wind speed ' WS '
!     210 residual NAPL saturation 'SNR '
!     211 mobile NAPL saturation 'SNM '
!     212 free NAPL saturation 'SNF '
!     213 surface temperature 'T_S'
!         or cricondentherm temperature ' TCT '
!     214 surface vapor pressure 'PV_S'
!     214 stress-yz, Pa
!     215 actual evaporation rate 'E_SA'
!     216 potential evaporation rate 'PE_SA'
!     217 actual transpiration rate 'T_SA'
!     218 potential transpiration rate 'PT_SA'
!     219 saturated co2 aqueous mass fraction 'SXLA'
!     220 aqueous alcohol mole fraction 'XMLA'
!         or nonaqueous liquid N2 mole fraction 'XMNN'
!     221 NAPL alcohol mode fraction 'XMNA'
!         or CO2 nonaqueous liquid mole fraction 'XMNA'
!     222 aqueous alcohol mass fraction 'XLA '
!         or nonaqueous N2 liquid mass fraction 'XNN'
!     223 NAPL alcohol mass fraction 'XNA '
!         or CO2 liquid-CO2 mass fraction 'XNA '
!     224 atmospheric pressure, ' PA '
!         or stress-xz, Pa
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 31 December 2003.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE OUTPU
      USE HYST
      USE GRID
      USE GEOMECH
      USE FILES
      USE FDVP
      USE FDVH
      USE FDVA
!

!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Parameter Statements----------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*16 FORM
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/WRPL_14'
      IF( INDEX(SVN_ID(277)(1:1),'$').EQ.0 ) SVN_ID(277) =
     & '$Id: wrplot.F 1080 2017-03-14 16:22:02Z d3c002 $' 
      VAR = 1.D+0
      IPNVGCX = IPNVGC
      IF( IPNV.EQ.210 ) THEN
        WRITE(IPL,'(/,A)') 'Residual-NAPL Saturation'
        WRITE(IPL,FORM) (SNR(2,N),N=1,NFX)
      ELSEIF( IPNV.EQ.211 ) THEN
        WRITE(IPL,'(/,A)') 'Mobile-NAPL Saturation'
        WRITE(IPL,FORM) ((SN(2,N)-SNT(2,N)-SNR(2,N)),N=1,NFX)
      ELSEIF( IPNV.EQ.212 ) THEN
        WRITE(IPL,'(/,A)') 'Free-NAPL Saturation'
        WRITE(IPL,FORM) ((SN(2,N)-SNT(2,N)),N=1,NFX)
      ELSEIF( IPNV.EQ.213 ) THEN
        WRITE(IPL,'(/,2A)') 'Cricondentherm Temperature, ',UNPLOT(IPNV)
        IF( UNPLOT(IPNV).EQ.'c' ) THEN
          WRITE(IPL,FORM) (TCT(N),N=1,NFX)
        ELSEIF( UNPLOT(IPNV).EQ.'k' ) THEN
          WRITE(IPL,FORM) ((TCT(N)+273.15D+0),N=1,NFX)
        ELSEIF( UNPLOT(IPNV).EQ.'f' ) THEN
          WRITE(IPL,FORM) ((TCT(N)*1.8D+0+3.2D+1),N=1,NFX)
        ELSEIF( UNPLOT(IPNV).EQ.'r' ) THEN
          WRITE(IPL,FORM) ((TCT(N)*1.8D+0+4.92D+2),N=1,NFX)
        ENDIF
      ELSEIF( IPNV.EQ.214 ) THEN
        INDX = 4
        IUNM = -1
        IUNKG = 1
        IUNS = -2
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
!
!---    Geomechanics  ---
!
        IF( ISLC(50).NE.0 ) THEN
          WRITE(IPL,'(/,2A)') 'Stress-yz, ',UNPLOT(IPNV)
          WRITE(IPL,FORM) (VAR*SIG_GM(4,N),N=1,NFX)
        ENDIF
      ELSEIF( IPNV.EQ.220 ) THEN
        IF( IOM.EQ.39 ) THEN
          WRITE(IPL,'(/,A)') 'Nonaqueous Liquid N2 Mole Fraction'
          WRITE(IPL,FORM) (XMNN(2,N),N=1,NFX)
        ENDIF
      ELSEIF( IPNV.EQ.221 ) THEN
          WRITE(IPL,'(/,A)')'Nonaqueous Liquid CO2 Mole Fraction '
          WRITE(IPL,FORM) (XMNA(2,N),N=1,NFX)
      ELSEIF( IPNV.EQ.222 ) THEN
        IF( IOM.EQ.39 ) THEN
          WRITE(IPL,'(/,A)') 'Nonaqueous Liquid N2 Mass Fraction'
          WRITE(IPL,FORM) (XNN(2,N),N=1,NFX)
        ELSE
          WRITE(IPL,'(/,A)') 'Aqueous Alcohol Mass Fraction'
          WRITE(IPL,FORM) (XLA(2,N),N=1,NFX)
        ENDIF
      ELSEIF( IPNV.EQ.223 ) THEN
        IF( IOM.GE.36 .AND. IOM.LE.38 ) THEN
          WRITE(IPL,'(/,A)')'Liquid-CO2 CO2 Mass Fraction '
        ELSEIF( IOM.EQ.39 ) THEN
          WRITE(IPL,'(/,A)')'Nonaqueous Liquid CO2 Mass Fraction '
        ELSE
          WRITE(IPL,'(/,A)')'NAPL Alcohol Mass Fraction '
        ENDIF
        WRITE(IPL,FORM) (XNA(2,N),N=1,NFX)
      ELSEIF( IPNV.EQ.224 ) THEN
        INDX = 4
        IUNM = -1
        IUNKG = 1
        IUNS = -2
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
!
!---    Geomechanics  ---
!
        IF( ISLC(50).NE.0 ) THEN
          WRITE(IPL,'(/,2A)') 'Stress-xz, ',UNPLOT(IPNV)
          WRITE(IPL,FORM) (VAR*SIG_GM(5,N),N=1,NFX)
        ENDIF
      ENDIF
      ISUB_LOG = ISUB_LOG-1
!
!---  End of WRPL_14 group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE WRPL_15( FORM,IPNV,IPNVGC,NFX )
!
!-------------------------Disclaimer-----------------------------------!
!
!     This material was prepared as an account of work sponsored by
!     an agency of the United States Government. Neither the
!     United States Government nor the United States Department of
!     Energy, nor Battelle, nor any of their employees, makes any
!     warranty, express or implied, or assumes any legal liability or
!     responsibility for the accuracy, completeness, or usefulness
!     of any information, apparatus, product, software or process
!     disclosed, or represents that its use would not infringe
!     privately owned rights.
!
!----------------------Acknowledgement---------------------------------!
!
!     This software and its documentation were produced with Government
!     support under Contract Number DE-AC06-76RLO-1830 awarded by the
!     United Department of Energy. The Government retains a paid-up
!     non-exclusive, irrevocable worldwide license to reproduce,
!     prepare derivative works, perform publicly and display publicly
!     by or for the Government, including the right to distribute to
!     other Government contractors.
!
!---------------------Copyright Notices--------------------------------!
!
!            Copyright Battelle Memorial Institute, 1996
!                    All Rights Reserved.
!
!----------------------Description-------------------------------------!
!
!     Write plot files for the following variables:
!
!     225 surface aqueous pressure, 'PL_S'
!     226 surface gas pressure, 'PG_S'
!         or stress-xy, Pa
!     227 surface aqueous saturation, 'SL_S
!     228 surface latent heat flux, 'QL_S'
!     229 surface sensible heat flux, 'QH_S'
!     230 surface net long-wave radiation, 'RL_S'
!     231 surface net short-wave radiation, 'RS_S'
!     232 surface net total radiation, 'RT_S'
!     233 surface water mass balance kg/s, 'WB_S'
!     234 plant temperature, 'T_P' or 'TPXX'
!     235 plant temperature, 'TPXX'
!     236 plant temperature, 'TPXX'
!     237 plant temperature, 'TPXX'
!     238 plant temperature, 'TPXX'
!     239 rainfall interception mass, 'RFIM'
!     240 sorbed oil mass, kg oil, 'TSO '
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 31 December 2003.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE PORMED
      USE OUTPU
      USE GRID
      USE GEOMECH
      USE FILES
      USE FDVP
      USE FDVN
!

!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Parameter Statements----------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*16 FORM
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/WRPL_15'
      IF( INDEX(SVN_ID(277)(1:1),'$').EQ.0 ) SVN_ID(277) =
     & '$Id: wrplot.F 1080 2017-03-14 16:22:02Z d3c002 $' 
      VAR = 1.D+0
      IPNVGCX = IPNVGC
      IF( IPNV.EQ.226 ) THEN
        INDX = 4
        IUNM = -1
        IUNKG = 1
        IUNS = -2
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
!
!---    Geomechanics  ---
!
        IF( ISLC(50).NE.0 ) THEN
          WRITE(IPL,'(/,2A)') 'Stress-xy, ',UNPLOT(IPNV)
          WRITE(IPL,FORM) (VAR*SIG_GM(6,N),N=1,NFX)
        ENDIF
      ELSEIF( IPNV.EQ.240 ) THEN
        INDX = 4
        IUNKG = 1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Sorbed Oil Mass, ',UNPLOT(IPNV)
        WRITE(IPL,FORM) (VAR*(VOL(N)*(1.D+0-PORD(2,N))
     &    *XSO(2,N)*RHOS(IZ(N))),N=1,NFX)
      ENDIF
      ISUB_LOG = ISUB_LOG-1
!
!---  End of WRPL_15 group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE WRPL_16( FORM,IPNV,IPNVGC,NFX )
!
!-------------------------Disclaimer-----------------------------------!
!
!     This material was prepared as an account of work sponsored by
!     an agency of the United States Government. Neither the
!     United States Government nor the United States Department of
!     Energy, nor Battelle, nor any of their employees, makes any
!     warranty, express or implied, or assumes any legal liability or
!     responsibility for the accuracy, completeness, or usefulness
!     of any information, apparatus, product, software or process
!     disclosed, or represents that its use would not infringe
!     privately owned rights.
!
!----------------------Acknowledgement---------------------------------!
!
!     This software and its documentation were produced with Government
!     support under Contract Number DE-AC06-76RLO-1830 awarded by the
!     United Department of Energy. The Government retains a paid-up
!     non-exclusive, irrevocable worldwide license to reproduce,
!     prepare derivative works, perform publicly and display publicly
!     by or for the Government, including the right to distribute to
!     other Government contractors.
!
!---------------------Copyright Notices--------------------------------!
!
!            Copyright Battelle Memorial Institute, 1996
!                    All Rights Reserved.
!
!----------------------Description-------------------------------------!
!
!     Write plot files for the following variables:
!
!     241 sorbed oil mass fraction kg oil/kg soil, 'XSO '
!         or hydrate N2 mass fraction 'XHN'
!     242 sorbed oil volumetric concentration kg oil/m^3, 'CSO '
!     247 x-direction intrinsic permeability m^2, ' UK '
!     247 matrix permeability m^2, 'KM' (STOMP-OS)
!     248 y-direction intrinsic permeability m^2, ' VK '
!     248 fracture permeability m^2, 'FM' (STOMP-OS)
!     249 z-direction intrinsic permeability m^2, ' WK '
!     251 hydrate CO2 mass fraction, 'XHA '
!     252 hydrate CH4 mass fraction, 'XHO '
!     253 hydrate density kg/m^3, 'RHOH'
!     254 hydrate saturation, ' SH '
!     255 hydrate pressure Pa, ' PH '
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 24 January 2004.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE PORMED
      USE OUTPU
      USE HYST
      USE GRID
      USE FILES
      USE FDVP
      USE FDVN
      USE FDVH
!

!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Parameter Statements----------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*16 FORM
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/WRPL_16'
      IF( INDEX(SVN_ID(277)(1:1),'$').EQ.0 ) SVN_ID(277) =
     & '$Id: wrplot.F 1080 2017-03-14 16:22:02Z d3c002 $' 
      VAR = 1.D+0
      IPNVGCX = IPNVGC
      IF( IPNV.EQ.241 ) THEN
        IF( IOM.EQ.39 ) THEN
          WRITE(IPL,'(/,A)') 'Hydrate N2 Mass Fraction'
          WRITE(IPL,FORM) (XHN(2,N),N=1,NFX)
        ELSE
          WRITE(IPL,'(/,A)') 'Sorbed Oil Mass Fraction'
          WRITE(IPL,FORM) (XSO(2,N),N=1,NFX)
        ENDIF
      ELSEIF( IPNV.EQ.242 ) THEN
        INDX = 4
        IUNKG = 1
        IUNM = -3
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Sorbed Oil Volumetric Conc., ',
     &    UNPLOT(IPNV)
        WRITE(IPL,FORM) (VAR*((1.D+0-PORD(2,N))
     &    *XSO(2,N)*RHOS(IZ(N))),N=1,NFX)
      ELSEIF( IPNV.EQ.247 ) THEN
        IF( IOM.EQ.50 .OR. IOM.EQ.51 .OR. IOM.EQ.52 ) THEN
          INDX = 4
          IUNM = 2
          CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
          WRITE(IPL,'(/,2A)')'Matrix Permeability, ',UNPLOT(IPNV)
          WRITE(IPL,FORM) (VAR*POSM(2,N),N=1,NFX)
        ELSEIF( IOM.EQ.32 .OR. IOM.EQ.33 ) THEN
          INDX = 4
          JNDX = 1
          IUNM = 2
          CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
          WRITE(IPL,'(/,2A)') 'X-Direction Intrinsic Permeability, ',
     &      UNPLOT(IPNV)
          WRITE(IPL,FORM) ((VAR*PERMV(JNDX,N)*PERMRF(2,N)),N=1,NFX)
        ELSE
          INDX = 4
          JNDX = 1
          IUNM = 2
          CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
          WRITE(IPL,'(/,2A)') 'X-Direction Intrinsic Permeability, ',
     &      UNPLOT(IPNV)
          WRITE(IPL,FORM) ((VAR*PERM(JNDX,IZ(N))*PERMRF(2,N)),N=1,NFX)
        ENDIF
      ELSEIF( IPNV.EQ.248 ) THEN
        IF( IOM.EQ.50 .OR. IOM.EQ.51 .OR. IOM.EQ.52 ) THEN
          INDX = 4
          IUNM = 2
          CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
          WRITE(IPL,'(/,2A)')'Fracture Permeability, ',UNPLOT(IPNV)
          WRITE(IPL,FORM) (VAR*PERMRF(2,N),N=1,NFX)
        ELSEIF( IOM.EQ.32 .OR. IOM.EQ.33 ) THEN
          INDX = 4
          JNDX = 2
          IUNM = 2
          CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
          WRITE(IPL,'(/,2A)') 'Y-Direction Intrinsic Permeability, ',
     &      UNPLOT(IPNV)
          WRITE(IPL,FORM) ((VAR*PERMV(JNDX,N)*PERMRF(2,N)),N=1,NFX)
        ELSE
          INDX = 4
          JNDX = 2
          IUNM = 2
          CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
          WRITE(IPL,'(/,2A)') 'Y-Direction Intrinsic Permeability, ',
     &      UNPLOT(IPNV)
          WRITE(IPL,FORM) ((VAR*PERM(JNDX,IZ(N))*PERMRF(2,N)),N=1,NFX)
        ENDIF
      ELSEIF( IPNV.EQ.249 ) THEN
        INDX = 4
        JNDX = 3
        IUNM = 2
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Z-Direction Intrinsic Permeability, ',
     &    UNPLOT(IPNV)
        IF( IOM.EQ.32 .OR. IOM.EQ.33 ) THEN
          WRITE(IPL,FORM) ((VAR*PERMV(JNDX,N)*PERMRF(2,N)),N=1,NFX)
        ELSE
          WRITE(IPL,FORM) ((VAR*PERM(JNDX,IZ(N))*PERMRF(2,N)),N=1,NFX)
        ENDIF
      ELSEIF( IPNV.EQ.250 ) THEN
        WRITE(IPL,'(/,A)') 'Hydrate Water Mass Fraction'
        WRITE(IPL,FORM) (XHW(2,N),N=1,NFX)
      ELSEIF( IPNV.EQ.251 ) THEN
        WRITE(IPL,'(/,A)') 'Hydrate CO2 Mass Fraction'
        WRITE(IPL,FORM) (XHA(2,N),N=1,NFX)
      ELSEIF( IPNV.EQ.252 ) THEN
        WRITE(IPL,'(/,A)') 'Hydrate CH4 Mass Fraction'
        WRITE(IPL,FORM) (XHO(2,N),N=1,NFX)
      ELSEIF( IPNV.EQ.253 ) THEN
        INDX = 4
        IUNM = -3
        IUNKG = 1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)')
     &    'Hydrate Density, ',UNPLOT(IPNV)
        WRITE(IPL,FORM) (VAR*RHOH(2,N),N=1,NFX)
      ELSEIF( IPNV.EQ.254 ) THEN
        WRITE(IPL,'(/,A)') 'Hydrate Saturation'
        WRITE(IPL,FORM) (SH(2,N),N=1,NFX)
      ELSEIF( IPNV.EQ.255 ) THEN
        INDX = 4
        IUNM = -1
        IUNKG = 1
        IUNS = -2
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Hydrate Pressure, ',UNPLOT(IPNV)
        WRITE(IPL,FORM) (VAR*PH(2,N),N=1,NFX)
      ELSEIF( IPNV.EQ.254 ) THEN
        WRITE(IPL,'(/,A)') 'Equilibrium Hydrate Saturation'
        WRITE(IPL,FORM) (ASGTN(N),N=1,NFX)
      ENDIF
      ISUB_LOG = ISUB_LOG-1
!
!---  End of WRPL_17 group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE WRPL_17( FORM,IPNV,IPNVGC,NFX )
!
!-------------------------Disclaimer-----------------------------------!
!
!     This material was prepared as an account of work sponsored by
!     an agency of the United States Government. Neither the
!     United States Government nor the United States Department of
!     Energy, nor Battelle, nor any of their employees, makes any
!     warranty, express or implied, or assumes any legal liability or
!     responsibility for the accuracy, completeness, or usefulness
!     of any information, apparatus, product, software or process
!     disclosed, or represents that its use would not infringe
!     privately owned rights.
!
!----------------------Acknowledgement---------------------------------!
!
!     This software and its documentation were produced with Government
!     support under Contract Number DE-AC06-76RLO-1830 awarded by the
!     United Department of Energy. The Government retains a paid-up
!     non-exclusive, irrevocable worldwide license to reproduce,
!     prepare derivative works, perform publicly and display publicly
!     by or for the Government, including the right to distribute to
!     other Government contractors.
!
!---------------------Copyright Notices--------------------------------!
!
!            Copyright Battelle Memorial Institute, 1996
!                    All Rights Reserved.
!
!----------------------Description-------------------------------------!
!
!     Write plot files for the following variables:
!
!     264 precipitated salt saturation, ' SS '
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 24 January 2004.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE OUTPU
      USE NAPL
      USE GRID
      USE FILES
      USE FDVS
      USE FDVH
      USE CONST
!

!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Parameter Statements----------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*16 FORM
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/WRPL_17'
      IF( INDEX(SVN_ID(277)(1:1),'$').EQ.0 ) SVN_ID(277) =
     & '$Id: wrplot.F 1080 2017-03-14 16:22:02Z d3c002 $' 
      VAR = 1.D+0
      IPNVGCX = IPNVGC
      IF( IPNV.EQ.264 ) THEN
        WRITE(IPL,'(/,2A)') 'Precipitated Salt Saturation, ',
     &    UNPLOT(IPNV)
        WRITE(IPL,FORM) (SS(2,N),N=1,NFX)
      ELSEIF( IPNV.EQ.265 ) THEN
        WRITE(IPL,'(/,A)') 'Hydrate Water Mole Fraction'
        WRITE(IPL,FORM) ((XHW(2,N)/WTMW)/
     &  ((XHA(2,N)/WTMA)+(XHO(2,N)/WTMO)+(XHW(2,N)/WTMW)),N=1,NFX)
      ELSEIF( IPNV.EQ.266 ) THEN
        WRITE(IPL,'(/,A)') 'Hydrate CO2 Mole Fraction'
        WRITE(IPL,FORM) ((XHA(2,N)/WTMA)/
     &  ((XHA(2,N)/WTMA)+(XHO(2,N)/WTMO)+(XHW(2,N)/WTMW)),N=1,NFX)
      ELSEIF( IPNV.EQ.267 ) THEN
        WRITE(IPL,'(/,A)') 'Hydrate CH4 Mole Fraction'
        WRITE(IPL,FORM) ((XHO(2,N)/WTMO)/
     &  ((XHA(2,N)/WTMA)+(XHO(2,N)/WTMO)+(XHW(2,N)/WTMW)),N=1,NFX)
      ENDIF
      ISUB_LOG = ISUB_LOG-1
!
!---  End of WRPL_17 group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE WRPL_18( FORM,IPNV,IPNVGC,NFX )
!
!-------------------------Disclaimer-----------------------------------!
!
!     This material was prepared as an account of work sponsored by
!     an agency of the United States Government. Neither the
!     United States Government nor the United States Department of
!     Energy, nor Battelle, nor any of their employees, makes any
!     warranty, express or implied, or assumes any legal liability or
!     responsibility for the accuracy, completeness, or usefulness
!     of any information, apparatus, product, software or process
!     disclosed, or represents that its use would not infringe
!     privately owned rights.
!
!----------------------Acknowledgement---------------------------------!
!
!     This software and its documentation were produced with Government
!     support under Contract Number DE-AC06-76RLO-1830 awarded by the
!     United Department of Energy. The Government retains a paid-up
!     non-exclusive, irrevocable worldwide license to reproduce,
!     prepare derivative works, perform publicly and display publicly
!     by or for the Government, including the right to distribute to
!     other Government contractors.
!
!---------------------Copyright Notices--------------------------------!
!
!            Copyright Battelle Memorial Institute, 1996
!                    All Rights Reserved.
!
!----------------------Description-------------------------------------!
!
!     Write plot files for the following variables:
!
!     264 precipitated salt saturation, ' SS '
!     275 well-node pressure (Pa), 'WNP'
!         or MCStress (Pa), 'MCSTR'
!     289 gas viscosity, 'VISG'
!     290 NAPL viscosity, 'VISN'
!     294 gas CH4 mole fraction of formers 'YMGO'
!     295 hydrate CH4 mole fraction of formers 'YMHO'
!     296 aqueous enthalpy, 'HL'
!     297 gas enthalpy, 'HG'
!     298 nonaqueous liquid phase enthalpy, 'HN'
!     299 similarity variable m^2/s, 'SIMV'
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 24 January 2004.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SPILL
      USE SOLTN
      USE OUTPU
      USE GRID
      USE FILES
      USE FDVT
      USE FDVP
      USE FDVN
      USE FDVH
      USE FDVG
      USE CONST
!

!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Parameter Statements----------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*16 FORM
      REAL*8, DIMENSION(:), ALLOCATABLE ::  DVAR
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/WRPL_18'
      IF( INDEX(SVN_ID(277)(1:1),'$').EQ.0 ) SVN_ID(277) =
     & '$Id: wrplot.F 1080 2017-03-14 16:22:02Z d3c002 $' 
!
!---  Dynamic memory allocation  ---
!
      ALLOCATE( DVAR(1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DVAR'
        CALL WRMSGS( INDX )
      ENDIF
      VAR = 1.D+0
      IPNVGCX = IPNVGC
!
!---  MCStress  ---
!
      IF( IPNV.EQ.275 ) THEN
        INDX = 4
        IUNM = -1
        IUNKG = 1
        IUNS = -2
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'MCStress, ',UNPLOT(IPNV)
        WRITE(IPL,FORM) (VAR*(PVO(1,N)),N=1,NFX)
      ELSEIF( IPNV.EQ.287 ) THEN
        DO 287 N = 1,NFX
          NSP = (JD(N)-1)*IFLD + ID(N)
          DVAR(N) = HLSP(2,NSP)
  287   CONTINUE
        INDX = 4
        IUNM = 1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Aqueous Spill Height, ',
     &    UNPLOT(IPNV)
        WRITE(IPL,FORM) ((VAR*DVAR(N)),N=1,NFX)
      ELSEIF( IPNV.EQ.288 ) THEN
        DO 288 N = 1,NFX
          NSP = (JD(N)-1)*IFLD + ID(N)
          DVAR(N) = HNSP(2,NSP)
  288   CONTINUE
        INDX = 4
        IUNM = 1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'NAPL Spill Height, ',
     &    UNPLOT(IPNV)
        WRITE(IPL,FORM) ((VAR*DVAR(N)),N=1,NFX)
!
!---  Gas viscosity  ---
!
      ELSEIF( IPNV.EQ.289 ) THEN
        INDX = 4
        IUNM = -1
        IUNKG = 1
        IUNS = -1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Gas Viscosity, ',UNPLOT(IPNV)
        WRITE(IPL,FORM) (VAR*VISG(2,N),N=1,NFX)
!
!---  NAPL viscosity  ---
!
      ELSEIF( IPNV.EQ.290 ) THEN
        INDX = 4
        IUNM = -1
        IUNKG = 1
        IUNS = -1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'NAPL Viscosity, ',UNPLOT(IPNV)
        WRITE(IPL,FORM) (VAR*VISN(2,N),N=1,NFX)
      ELSEIF( IPNV.EQ.291 ) THEN
        INDX = 4
        IUNM = 1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'X Node-Centroid Position, ',
     &    UNPLOT(IPNV)
        WRITE(IPL,FORM) ((VAR*XP(N)),N=1,NFX)
      ELSEIF( IPNV.EQ.292 ) THEN
        INDX = 4
        IUNM = 1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Y Node-Centroid Position, ',
     &    UNPLOT(IPNV)
        WRITE(IPL,FORM) ((VAR*YP(N)),N=1,NFX)
      ELSEIF( IPNV.EQ.293 ) THEN
        INDX = 4
        IUNM = 1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Z Node-Centroid Position, ',
     &    UNPLOT(IPNV)
        WRITE(IPL,FORM) ((VAR*ZP(N)),N=1,NFX)
      ELSEIF( IPNV.EQ.294 ) THEN
        WRITE(IPL,'(/,A)') 'Gas CH4 Mole Fraction of Formers'
        WRITE(IPL,FORM) (YMGO(2,N),N=1,NFX)
      ELSEIF( IPNV.EQ.295 ) THEN
        WRITE(IPL,'(/,A)') 'Hydrate CH4 Mole Fraction of Formers'
        WRITE(IPL,FORM) (YMHGO(2,N),N=1,NFX)
!
!---  Aqueous enthalpy  ---
!
      ELSEIF( IPNV.EQ.296 ) THEN
        INDX = 4
        IUNM = 2
        IUNS = -2
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Aqueous Enthalpy, ',UNPLOT(IPNV)
        WRITE(IPL,FORM) (VAR*HL(2,N),N=1,NFX)
!
!---  Gas enthalpy  ---
!
      ELSEIF( IPNV.EQ.297 ) THEN
        INDX = 4
        IUNM = 2
        IUNS = -2
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Gas Enthalpy, ',UNPLOT(IPNV)
        WRITE(IPL,FORM) (VAR*HG(2,N),N=1,NFX)
!
!---  NAPL enthalpy  ---
!
      ELSEIF( IPNV.EQ.298 ) THEN
        INDX = 4
        IUNM = 2
        IUNS = -2
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'NAPL Enthalpy, ',UNPLOT(IPNV)
        WRITE(IPL,FORM) (VAR*HN(2,N),N=1,NFX)
!
!---  Similarity variable  ---
!
      ELSEIF( IPNV.EQ.299 ) THEN
        INDX = 4
        IUNM = 2
        IUNS = -1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        DO 299 N = 1,NFX
          DVAR(N) = (XP(N)**2)/(TM+SMALL)
  299   CONTINUE
        WRITE(IPL,'(/,2A)') 'Similarity Variable, ',UNPLOT(IPNV)
        WRITE(IPL,FORM) ((VAR*DVAR(N)),N=1,NFX)
      ENDIF
!
!---  Deallocate memory  ---
!
      DEALLOCATE( DVAR,STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Deallocation Error: DVAR'
        CALL WRMSGS( INDX )
      ENDIF
      ISUB_LOG = ISUB_LOG-1
!
!---  End of WRPL_18 group
!
      RETURN
      END


!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE WRPL_20( FORM,IPNV,IPNVGC,NFX )
!
!-------------------------Disclaimer-----------------------------------!
!
!     This material was prepared as an account of work sponsored by
!     an agency of the United States Government. Neither the
!     United States Government nor the United States Department of
!     Energy, nor Battelle, nor any of their employees, makes any
!     warranty, express or implied, or assumes any legal liability or
!     responsibility for the accuracy, completeness, or usefulness
!     of any information, apparatus, product, software or process
!     disclosed, or represents that its use would not infringe
!     privately owned rights.
!
!----------------------Acknowledgement---------------------------------!
!
!     This software and its documentation were produced with Government
!     support under Contract Number DE-AC06-76RLO-1830 awarded by the
!     United Department of Energy. The Government retains a paid-up
!     non-exclusive, irrevocable worldwide license to reproduce,
!     prepare derivative works, perform publicly and display publicly
!     by or for the Government, including the right to distribute to
!     other Government contractors.
!
!---------------------Copyright Notices--------------------------------!
!
!            Copyright Battelle Memorial Institute, 1996
!                    All Rights Reserved.
!
!----------------------Description-------------------------------------!
!
!     Write plot files for the following variables:
!
!     341 aqueous internal energy, 'UL'
!     342 gas internal energy, 'UG'
!     343 nonaqueous liquid phase internal energy, 'UN'
!     344 aqueous thermal conductivity, 'THKL'
!     345 gas thermal conductivity, 'THKG'
!     346 nonaqueous liquid phase thermal conductivity, 'THKN'
!     347 CO2 aqueous diffusion coefficient, 'DFLA' 
!     348 H2O gas diffusion coefficient, 'DFGW' 
!     349 coupled-well CO2 mass rate, kg/s 'QMRA'
!     350 coupled-well CO2 mass integral, kg 'QMIA'
!     351 coupled-well water mass rate, kg/s 'QMRW'
!     352 coupled-well water mass integral, kg 'QMIW'
!     361 vertically-integrated CO2 mass, kg, VIMA
!     362 vertically-integrated CO2 mass, kg/m^2, VIAPA
!     363 vertically-integrated CO2 mass, kg, VIGA
!     364 vertically-integrated CO2 mass, kg/m^2, VIGAPA
!     365 vertically-integrated CO2 mass, kg, VILA
!     366 vertically-integrated CO2 mass, kg/m^2, VILAPA
!     367 integrated precipitated salt mass, kg, IMPS
!     368 mean effective stress, Pa, STRS-M
!     369 strain-xx
!     370 strain-yy
!     371 strain-zz
!     372 strain-yz
!     373 strain-xz
!     374 strain-xy
!     375 x displacement, m, DISPX
!     376 y displacement, m, DISPY
!     377 z displacement, m, DISPZ
!     378 integrated energy, J, 'IQ'
!     379 gas CO2 mole fraction of formers, 'YMGA'
!     380 hydrate-gas CO2 mole fraction of former,s 'YMHGA'
!     381 gas N2 mole fraction of formers, 'YMGN'
!     382 hydrate-gas N2 mole fraction of formers, 'YMHGN'
!     383 total nonaqueous CO2 mole fraction of formers, 'ZMCA'
!         or total petroleum component mole fraction, 'ZMC'
!     384 total nonaqueous CH4 mole fraction of formers, 'ZMCO'
!     385 total nonaqueous N2 mole fraction of formers, 'ZMCN'
!     388 x-direction index of node, 'I'
!     389 y-direction index of node, 'J'
!     390 z-direction index of node, 'K'
!     391 xnc surface area, m^2 'AFX'
!     392 ync surface area, m^2 'AFY'
!     393 znc surface area, m^2 'AFZ'
!     394 saturation function index, 'SFZN'
!     395 processor id, 'PID'
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 22 December 2010.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE OUTPU
      USE GRID
      USE GEOMECH
      USE FILES
      USE FDVT
      USE FDVP
      USE FDVH
      USE FDVGC
      USE FDVG
      USE FDVA
      USE CONST
!

!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Parameter Statements----------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*16 FORM
      REAL*8, DIMENSION(:), ALLOCATABLE ::  DVAR
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/WRPL_20'
      IF( INDEX(SVN_ID(277)(1:1),'$').EQ.0 ) SVN_ID(277) =
     & '$Id: wrplot.F 1080 2017-03-14 16:22:02Z d3c002 $' 
!
!---  Dynamic memory allocation  ---
!
      ALLOCATE( DVAR(1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DVAR'
        CALL WRMSGS( INDX )
      ENDIF
      VAR = 1.D+0
      IPNVGCX = IPNVGC
!
!---  Aqueous internal energy  ---
!
      IF( IPNV.EQ.341 ) THEN
        INDX = 4
        IUNM = 2
        IUNS = -2
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Aqueous Internal Energy, ',UNPLOT(IPNV)
        DO 341 N = 1,NFX
          DVAR(N) = VAR*(HL(2,N)-(PG(2,N)+PATM)/RHOL(2,N))
  341   CONTINUE
        WRITE(IPL,FORM) (DVAR(N),N=1,NFX)
!
!---  Gas internal energy  ---
!
      ELSEIF( IPNV.EQ.342 ) THEN
        INDX = 4
        IUNM = 2
        IUNS = -2
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Gas Internal Energy, ',UNPLOT(IPNV)
        WRITE(IPL,FORM) (VAR*UEG(2,N),N=1,NFX)
!
!---  NAPL internal energy  ---
!
      ELSEIF( IPNV.EQ.343 ) THEN
        INDX = 4
        IUNM = 2
        IUNS = -2
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'NAPL Internal Energy, ',UNPLOT(IPNV)
        DO 343 N = 1,NFX
          DVAR(N) = VAR*(HN(2,N)-(PG(2,N)+PATM)/RHON(2,N))
  343   CONTINUE
        WRITE(IPL,FORM) (DVAR(N),N=1,NFX)
!
!---  Aqueous thermal conductivity  ---
!
      ELSEIF( IPNV.EQ.344 ) THEN
        INDX = 4
        IUNKG = 1
        IUNM = 1
        IUNS = -3
        IUNK = -1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Aqueous Thermal Conductivity, ',
     &    UNPLOT(IPNV)
        WRITE(IPL,FORM) (VAR*THKL(2,N),N=1,NFX)
!
!---  Gas thermal conductivity  ---
!
      ELSEIF( IPNV.EQ.345 ) THEN
        INDX = 4
        IUNKG = 1
        IUNM = 1
        IUNS = -3
        IUNK = -1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Gas Thermal Conductivity, ',
     &    UNPLOT(IPNV)
        WRITE(IPL,FORM) (VAR*THKG(2,N),N=1,NFX)
!
!---  NAPL thermal conductivity  ---
!
      ELSEIF( IPNV.EQ.346 ) THEN
        INDX = 4
        IUNKG = 1
        IUNM = 1
        IUNS = -3
        IUNK = -1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'NAPL Thermal Conductivity, ',
     &    UNPLOT(IPNV)
        WRITE(IPL,FORM) (VAR*THKN(2,N),N=1,NFX)
!
!---  CO2 aqueous diffusion coefficient  ---
!
      ELSEIF( IPNV.EQ.347 ) THEN
        INDX = 4
        IUNM = 2
        IUNS = -1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'CO2 Aqueous Diffusion Coefficient, ',
     &    UNPLOT(IPNV)
        WRITE(IPL,FORM) (VAR*DFLA(2,N),N=1,NFX)
!
!---  H2O gas diffusion coefficient  ---
!
      ELSEIF( IPNV.EQ.348 ) THEN
        INDX = 4
        IUNM = 2
        IUNS = -1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'H2O Gas Diffusion Coefficient, ',
     &    UNPLOT(IPNV)
        WRITE(IPL,FORM) (VAR*DFGW(2,N),N=1,NFX)
!
!---  Vertically-integrated CO2 mass  ---
!
      ELSEIF( IPNV.EQ.361 ) THEN
        INDX = 4
        IUNKG = 1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Vertically-Integrated CO2 Mass, ' 
     &    ,UNPLOT(IPNV)
        DO 1361 JX = 1,JFLD
        DO 1361 IX = 1,IFLD
          N = ND(IX,JX,1)
          DVAR(N) = 0.D+0
          DO 361 KX = 1,KFLD
            NX = ND(IX,JX,KX)
            IF( IXP(NX).EQ.0 ) GOTO 361
            DVAR(N) = DVAR(N) + VOL(NX)*PORD(2,NX)*
     &        (SL(2,NX)*RHOL(2,NX)*XLA(2,NX) +
     &         SG(2,NX)*RHOG(2,NX)*XGA(2,NX))
            IF( INAPL.EQ.1 ) DVAR(N) = DVAR(N) + VOL(NX)*PORD(2,NX)*
     &        SN(2,NX)*RHON(2,NX)*XNA(2,NX)
  361     CONTINUE
          DVAR(N) = VAR*DVAR(N)
 1361   CONTINUE
        DO 2361 KX = 2,KFLD
        DO 2361 JX = 1,JFLD
        DO 2361 IX = 1,IFLD
          N = ND(IX,JX,1)
          NX = ND(IX,JX,KX)
          DVAR(NX) = DVAR(N)
 2361   CONTINUE
        WRITE(IPL,FORM) (DVAR(N),N=1,NFX)
!
!---  Vertically-integrated CO2 mass per area  ---
!
      ELSEIF( IPNV.EQ.362 ) THEN
        INDX = 4
        IUNKG = 1
        IUNM = -2 
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Vertically-Integrated CO2 Mass per Area, ' 
     &    ,UNPLOT(IPNV)
        DO 1362 JX = 1,JFLD
        DO 1362 IX = 1,IFLD
          N = ND(IX,JX,1)
          DVAR(N) = 0.D+0
          DO 362 KX = 1,KFLD
            NX = ND(IX,JX,KX)
            IF( IXP(NX).EQ.0 ) GOTO 362
            DVAR(N) = DVAR(N) + VOL(NX)*PORD(2,NX)*
     &        (SL(2,NX)*RHOL(2,NX)*XLA(2,NX) +
     &         SG(2,NX)*RHOG(2,NX)*XGA(2,NX))/AFZ(NSZ(NX))
            IF( INAPL.EQ.1 ) DVAR(N) = DVAR(N) + VOL(NX)*PORD(2,NX)*
     &        SN(2,NX)*RHON(2,NX)*XNA(2,NX)/AFZ(NSZ(NX))
  362     CONTINUE
          DVAR(N) = VAR*DVAR(N)
 1362   CONTINUE
        DO 2362 KX = 2,KFLD
        DO 2362 JX = 1,JFLD
        DO 2362 IX = 1,IFLD
          N = ND(IX,JX,1)
          NX = ND(IX,JX,KX)
          DVAR(NX) = DVAR(N)
 2362   CONTINUE
        WRITE(IPL,FORM) (DVAR(N),N=1,NFX)
!
!---  Vertically-integrated gas CO2 mass  ---
!
      ELSEIF( IPNV.EQ.363 ) THEN
        INDX = 4
        IUNKG = 1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Vertically-Integrated Gas CO2 Mass, ' 
     &    ,UNPLOT(IPNV)
        DO 1363 JX = 1,JFLD
        DO 1363 IX = 1,IFLD
          N = ND(IX,JX,1)
          DVAR(N) = 0.D+0
          DO 363 KX = 1,KFLD
            NX = ND(IX,JX,KX)
            IF( IXP(NX).EQ.0 ) GOTO 363
            DVAR(N) = DVAR(N) + VOL(NX)*PORD(2,NX)*
     &        SG(2,NX)*RHOG(2,NX)*XGA(2,NX)
  363     CONTINUE
          DVAR(N) = VAR*DVAR(N)
 1363   CONTINUE
        DO 2363 KX = 2,KFLD
        DO 2363 JX = 1,JFLD
        DO 2363 IX = 1,IFLD
          N = ND(IX,JX,1)
          NX = ND(IX,JX,KX)
          DVAR(NX) = DVAR(N)
 2363   CONTINUE
        WRITE(IPL,FORM) (DVAR(N),N=1,NFX)
!
!---  Vertically-integrated gas CO2 mass per area  ---
!
      ELSEIF( IPNV.EQ.364 ) THEN
        INDX = 4
        IUNKG = 1
        IUNM = -2 
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Vertically-Integrated Gas ' //  
     &    'CO2 Mass per Area, ',UNPLOT(IPNV)
        DO 1364 JX = 1,JFLD
        DO 1364 IX = 1,IFLD
          N = ND(IX,JX,1)
          DVAR(N) = 0.D+0
          DO 364 KX = 1,KFLD
            NX = ND(IX,JX,KX)
            IF( IXP(NX).EQ.0 ) GOTO 364
            DVAR(N) = DVAR(N) + VOL(NX)*PORD(2,NX)*
     &        SG(2,NX)*RHOG(2,NX)*XGA(2,NX)/AFZ(NSZ(NX))
  364     CONTINUE
          DVAR(N) = VAR*DVAR(N)
 1364   CONTINUE
        DO 2364 KX = 2,KFLD
        DO 2364 JX = 1,JFLD
        DO 2364 IX = 1,IFLD
          N = ND(IX,JX,1)
          NX = ND(IX,JX,KX)
          DVAR(NX) = DVAR(N)
 2364   CONTINUE
        WRITE(IPL,FORM) (DVAR(N),N=1,NFX)
!
!---  Vertically-integrated aqueous CO2 mass  ---
!
      ELSEIF( IPNV.EQ.365 ) THEN
        INDX = 4
        IUNKG = 1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Vertically-Integrated Aqueous CO2 Mass, ' 
     &    ,UNPLOT(IPNV)
        DO 1365 JX = 1,JFLD
        DO 1365 IX = 1,IFLD
          N = ND(IX,JX,1)
          DVAR(N) = 0.D+0
          DO 365 KX = 1,KFLD
            NX = ND(IX,JX,KX)
            IF( IXP(NX).EQ.0 ) GOTO 365
            DVAR(N) = DVAR(N) + VOL(NX)*PORD(2,NX)*
     &        SL(2,NX)*RHOL(2,NX)*XLA(2,NX)
  365     CONTINUE
          DVAR(N) = VAR*DVAR(N)
 1365   CONTINUE
        DO 2365 KX = 2,KFLD
        DO 2365 JX = 1,JFLD
        DO 2365 IX = 1,IFLD
          N = ND(IX,JX,1)
          NX = ND(IX,JX,KX)
          DVAR(NX) = DVAR(N)
 2365   CONTINUE
        WRITE(IPL,FORM) (DVAR(N),N=1,NFX)
!
!---  Vertically-integrated aqueous CO2 mass per area  ---
!
      ELSEIF( IPNV.EQ.366 ) THEN
        INDX = 4
        IUNKG = 1
        IUNM = -2 
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Vertically-Integrated Aqueous ' //  
     &    'CO2 Mass per Area, ',UNPLOT(IPNV)
        DO 1366 JX = 1,JFLD
        DO 1366 IX = 1,IFLD
          N = ND(IX,JX,1)
          DVAR(N) = 0.D+0
          DO 366 KX = 1,KFLD
            NX = ND(IX,JX,KX)
            IF( IXP(NX).EQ.0 ) GOTO 366
            DVAR(N) = DVAR(N) + VOL(NX)*PORD(2,NX)*
     &        SL(2,NX)*RHOL(2,NX)*XLA(2,NX)/AFZ(NSZ(NX))
  366     CONTINUE
          DVAR(N) = VAR*DVAR(N)
 1366   CONTINUE
        DO 2366 KX = 2,KFLD
        DO 2366 JX = 1,JFLD
        DO 2366 IX = 1,IFLD
          N = ND(IX,JX,1)
          NX = ND(IX,JX,KX)
          DVAR(NX) = DVAR(N)
 2366   CONTINUE
        WRITE(IPL,FORM) (DVAR(N),N=1,NFX)
!
!---  Strain-xx  ---
!
      ELSEIF( IPNV.EQ.369 ) THEN
        WRITE(IPL,'(/,A)') 'Strain-xx '
        WRITE(IPL,FORM) (EPS_GM(1,N),N=1,NFX)
!
!---  Strain-yy  ---
!
      ELSEIF( IPNV.EQ.370 ) THEN
        WRITE(IPL,'(/,A)') 'Strain-yy '
        WRITE(IPL,FORM) (EPS_GM(2,N),N=1,NFX)
!
!---  Strain-zz  ---
!
      ELSEIF( IPNV.EQ.371 ) THEN
        WRITE(IPL,'(/,A)') 'Strain-zz '
        WRITE(IPL,FORM) (EPS_GM(3,N),N=1,NFX)
!
!---  Strain-yz  ---
!
      ELSEIF( IPNV.EQ.372 ) THEN
        WRITE(IPL,'(/,A)') 'Strain-yz '
        WRITE(IPL,FORM) (EPS_GM(4,N),N=1,NFX)
!
!---  Strain-xz  ---
!
      ELSEIF( IPNV.EQ.373 ) THEN
        WRITE(IPL,'(/,A)') 'Strain-xz '
        WRITE(IPL,FORM) (EPS_GM(5,N),N=1,NFX)
!
!---  Strain-xy  ---
!
      ELSEIF( IPNV.EQ.374 ) THEN
        WRITE(IPL,'(/,A)') 'Strain-xy '
        WRITE(IPL,FORM) (EPS_GM(6,N),N=1,NFX)
!
!---  X Displacement  ---
!
      ELSEIF( IPNV.EQ.375 ) THEN
        INDX = 4
        IUNM = 1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        IF( IPNVGC.EQ.0 ) THEN
          WRITE(IPL,'(/,2A)') 'X-Displacement, ',UNPLOT(IPNV)
          DO N = 1,NFX
            DVAR(N) = 0.D+0
            IF( IXP(N).EQ.0 ) CYCLE
            DO L = 1,8
              NFEN = ND_GM(L,N)
              DVAR(N) = DVAR(N) + VAR*(U_GM(2,NFEN)-U_GM(1,NFEN))
            ENDDO
            DVAR(N) = 1.25D-1*DVAR(N)
          ENDDO
        ELSE
          WRITE(IPL,'(/,A,I1,2A)') 'X-Displacement: FE-Node: ',IPNVGC,
     &      ': ',UNPLOT(IPNV)
          DO N = 1,NFX
            DVAR(N) = 0.D+0
            IF( IXP(N).EQ.0 ) CYCLE
            NFEN = ND_GM(IPNVGC,N)
            DVAR(N) = VAR*(U_GM(2,NFEN)-U_GM(1,NFEN))
          ENDDO
        ENDIF
        WRITE(IPL,FORM) (DVAR(N),N=1,NFX)
!
!---  Y Displacement  ---
!
      ELSEIF( IPNV.EQ.376 ) THEN
        INDX = 4
        IUNM = 1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        IF( IPNVGC.EQ.0 ) THEN
          WRITE(IPL,'(/,2A)') 'Y-Displacement, ',UNPLOT(IPNV)
          DO N = 1,NFX
            DVAR(N) = 0.D+0
            IF( IXP(N).EQ.0 ) CYCLE
            DO L = 1,8
              NFEN = ND_GM(L,N)
              DVAR(N) = DVAR(N) + VAR*(V_GM(2,NFEN)-V_GM(1,NFEN))
            ENDDO
            DVAR(N) = 1.25D-1*DVAR(N)
          ENDDO
        ELSE
          WRITE(IPL,'(/,A,I1,2A)') 'Y-Displacement: FE-Node: ',IPNVGC,
     &      ': ',UNPLOT(IPNV)
          DO N = 1,NFX
            DVAR(N) = 0.D+0
            IF( IXP(N).EQ.0 ) CYCLE
            NFEN = ND_GM(IPNVGC,N)
            DVAR(N) = VAR*(V_GM(2,NFEN)-V_GM(1,NFEN))
          ENDDO
        ENDIF
        WRITE(IPL,FORM) (DVAR(N),N=1,NFX)
!
!---  Z Displacement  ---
!
      ELSEIF( IPNV.EQ.377 ) THEN
        INDX = 4
        IUNM = 1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        IF( IPNVGC.EQ.0 ) THEN
          WRITE(IPL,'(/,2A)') 'Z-Displacement, ',UNPLOT(IPNV)
          DO N = 1,NFX
            DVAR(N) = 0.D+0
            IF( IXP(N).EQ.0 ) CYCLE
            DO L = 1,8
              NFEN = ND_GM(L,N)
              DVAR(N) = DVAR(N) + VAR*(W_GM(2,NFEN)-W_GM(1,NFEN))
            ENDDO
            DVAR(N) = 1.25D-1*DVAR(N)
          ENDDO
        ELSE
          WRITE(IPL,'(/,A,I1,2A)') 'Z-Displacement: FE-Node: ',IPNVGC,
     &      ': ',UNPLOT(IPNV)
          DO N = 1,NFX
            DVAR(N) = 0.D+0
            IF( IXP(N).EQ.0 ) CYCLE
            NFEN = ND_GM(IPNVGC,N)
            DVAR(N) = VAR*(W_GM(2,NFEN)-W_GM(1,NFEN))
          ENDDO
        ENDIF
        WRITE(IPL,FORM) (DVAR(N),N=1,NFX)
!
!---  Gas CO2 mole fraction of formers  ---
!
      ELSEIF( IPNV.EQ.379 ) THEN
        WRITE(IPL,'(/,A)') 'Gas CO2 Mole Fraction of Formers'
        WRITE(IPL,FORM) (YMGA(2,N),N=1,NFX)
!
!---  Hydrate-gas CO2 mole fraction of formers  ---
!
      ELSEIF( IPNV.EQ.380 ) THEN
        WRITE(IPL,'(/,A)') 'Hydrate-Gas CO2 Mole Fraction of Formers'
        WRITE(IPL,FORM) (YMHGA(2,N),N=1,NFX)
!
!---  Gas N2 mole fraction of formers  ---
!
      ELSEIF( IPNV.EQ.381 ) THEN
        WRITE(IPL,'(/,A)') 'Gas N2 Mole Fraction of Formers'
        WRITE(IPL,FORM) (YMGN(2,N),N=1,NFX)
!
!---  Hydrate-gas N2 mole fraction of formers  ---
!
      ELSEIF( IPNV.EQ.382 ) THEN
        WRITE(IPL,'(/,A)') 'Hydrate-Gas N2 Mole Fraction of Formers'
        WRITE(IPL,FORM) (YMHGN(2,N),N=1,NFX)
!
!---  Total nonaqueous liquid CO2 mole fraction of formers  ---
!
      ELSEIF( IPNV.EQ.383 ) THEN
        IF( IOM.EQ.43 ) THEN
          NCHX = INDEX(GCNM(IPNVGC)(1:),'  ')-1
          WRITE(IPL,'(/,A)') 'Total Petroleum Component ' // 
     &      GCNM(IPNVGC)(1:NCHX) // ' Mole Fraction'
          WRITE(IPL,FORM) (ZMC(IPNVGC,2,N),N=1,NFX)
        ELSE
          WRITE(IPL,'(/,A)') 'Total Nonaqueous CO2 Mole ' // 
     &      'Fraction of Formers'
          WRITE(IPL,FORM) (ZMCA(2,N),N=1,NFX)
        ENDIF
!
!---  Total nonaqueous liquid CH4 mole fraction of formers  ---
!
      ELSEIF( IPNV.EQ.384 ) THEN
        WRITE(IPL,'(/,A)') 'Total Nonaqueous CH4 Mole ' // 
     &    'Fraction of Formers'
        WRITE(IPL,FORM) (ZMCO(2,N),N=1,NFX)
!
!---  Total nonaqueous liquid N2 mole fraction of formers  ---
!
      ELSEIF( IPNV.EQ.385 ) THEN
        WRITE(IPL,'(/,A)') 'Total Nonaqueous N2 Mole ' // 
     &    'Fraction of Formers'
        WRITE(IPL,FORM) (ZMCN(2,N),N=1,NFX)
!
!---  Hydrate equilibrium pressure  ---
!
      ELSEIF( IPNV.EQ.386 ) THEN
        WRITE(IPL,'(/,A)') 'Hydrate Equilibrim Pressure'
        WRITE(IPL,FORM) ((PVHA(2,N)+PVHO(2,N)+PVHN(2,N)),N=1,NFX)
!
!---  Total mobile former vapor pressure  ---
!
      ELSEIF( IPNV.EQ.387 ) THEN
        WRITE(IPL,'(/,A)') 'Total Mobile Formers Vapor Pressure'
        WRITE(IPL,FORM) ((PVA(2,N)+PVO(2,N)+PVN(2,N)),N=1,NFX)
!
!---  X-Direction Node Index  ---
!
      ELSEIF( IPNV.EQ.388 ) THEN
        WRITE(IPL,'(/,A)') 'I Node Index'
        WRITE(IPL,FORM) (REAL(ID(N)),N=1,NFX)
!
!---  Y-Direction Node Index  ---
!
      ELSEIF( IPNV.EQ.389 ) THEN
        WRITE(IPL,'(/,A)') 'J Node Index'
        WRITE(IPL,FORM) (REAL(JD(N)),N=1,NFX)
!
!---  Z-Direction Node Index  ---
!
      ELSEIF( IPNV.EQ.390 ) THEN
        WRITE(IPL,'(/,A)') 'K Node Index'
        WRITE(IPL,FORM) (REAL(KD(N)),N=1,NFX)
!
!---  X-Direction Node Centered Surface Area  ---
!
      ELSEIF( IPNV.EQ.391 ) THEN
        INDX = 4
        IUNM = 2
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'X-Dir. Surface Area ' //
     &    '(Node Centered), ',UNPLOT(IPNV)
        DO N = 1,NFX
          IF( IXP(N).EQ.0 ) THEN
            DVAR(N) = 0.D+0
          ELSE
            DVAR(N) = VAR*0.5D+0*(AFX(NSX(N))+AFX(NSX(N)+1))
          ENDIF
        ENDDO
        WRITE(IPL,FORM) (DVAR(N),N=1,NFX)
!
!---  Y-Direction Node Centered Surface Area  ---
!
      ELSEIF( IPNV.EQ.392 ) THEN
        INDX = 4
        IUNM = 2
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Y-Dir. Surface Area ' //
     &    '(Node Centered), ',UNPLOT(IPNV)
        DO N = 1,NFX
          IF( IXP(N).EQ.0 ) THEN
            DVAR(N) = 0.D+0
          ELSE
            DVAR(N) = VAR*0.5D+0*(AFY(NSY(N))+AFY(NSY(N)+IFLD))
          ENDIF
        ENDDO
        WRITE(IPL,FORM) (DVAR(N),N=1,NFX)
!
!---  Z-Direction Node Centered Surface Area  ---
!
      ELSEIF( IPNV.EQ.393 ) THEN
        INDX = 4
        IUNM = 2
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Z-Dir. Surface Area ' //
     &    '(Node Centered), ',UNPLOT(IPNV)
        DO N = 1,NFX
          IF( IXP(N).EQ.0 ) THEN
            DVAR(N) = 0.D+0
          ELSE
            DVAR(N) = VAR*0.5D+0*(AFZ(NSZ(N))+AFZ(NSZ(N)+IJFLD))
          ENDIF
        ENDDO
        WRITE(IPL,FORM) (DVAR(N),N=1,NFX)
!
!---  Saturation function index  ---
!
      ELSEIF( IPNV.EQ.394 ) THEN
        WRITE(IPL,'(/,A)') 'Saturation Function Index'
        DO 394 N = 1,NFX
          DVAR(N) = REAL(IZ2(N))
          IF( IXP(N).EQ.0 ) DVAR(N) = 0.D+0
  394   CONTINUE
        WRITE(IPL,FORM) (DVAR(N),N=1,NFX)
!
!---  Gas-nonaqueous liquid interfacial tension  ---
!
      ELSEIF( IPNV.EQ.396 ) THEN
        INDX = 4
        IUNKG = 1
        IUNS = -2
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Gas-Nonaqueous Liquid Interfacial ' //
     &    'Tension, ',UNPLOT(IPNV)
        WRITE(IPL,FORM) (VAR*GNIFT(2,N),N=1,NFX)
      ENDIF
!
!---  Deallocate memory  ---
!
      DEALLOCATE( DVAR,STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Deallocation Error: DVAR'
        CALL WRMSGS( INDX )
      ENDIF
      ISUB_LOG = ISUB_LOG-1
!
!---  End of WRPL_20 group
!
      RETURN
      END



