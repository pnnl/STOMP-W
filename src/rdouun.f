!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDOUUN( INDX )
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
!     Read reference node and plot file output variable units.
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
!   81 ice saturation (null), 'SI '
!   82 ice density (kg/m^3), 'RHOF'
!   83 aqueous matrix (null), 'DSLM'
!       or NAPL matrix (null), 'DSNM'
!   84 aqueous fracture (null), 'DSLF'
!       or NAPL fracture (null), 'DSNF'
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
!   225 surface aqueous pressure (Pa), 'PL_S'
!   226 surface gas pressure (Pa), 'PG_S'
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
!   396 gas-nonaqueous liquid interfacial tension, N/m 'IFT'
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
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNL, November, 1995.
!     Last Modified by MD White, Battelle, November 21, 1995.
!     $Id: rdouun.F 1080 2017-03-14 16:22:02Z d3c002 $
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE TRNSPT
      USE SOLTN
!

!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Parameter Statements----------------------------!
!



      PARAMETER (LUNS=400+33+33)
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      INTEGER IUM(LUNS),IUKG(LUNS),IUS(LUNS),IUK(LUNS),IUMOL(LUNS)
      SAVE IUM,IUKG,IUS,IUK,IUMOL
!
!----------------------Data Statements---------------------------------!
!
!        1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
      DATA IUM /
     &  -1,-1,-1, 0, 0,-1,-1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0,-3,-3,-3, 0, 0, 0, 0,
     &   0, 0, 2, 1, 1, 1,-3,-3, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0,
     &   0, 0, 1,-2,-2,-2,-2,-2,-2, 0, 0, 0,-3,-3,-3,-3,-3,-3, 0,-1,
     &   0,-3, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0,
     &  -1, 0,-3,-3, 0, 0, 0, 0,-3, 0,-3,-3, 0,-2,-2,-2,-2,-2,-2,-2,
     &  -2,-2,-2,-2,-2, 0, 0,-1,-1,-1, 0,-1,-1, 0, 0, 0, 0,-1,-3, 0,
     &   0, 0, 2, 1, 3, 3, 0, 0, 0, 0, 0,-3,-3, 3, 3, 3, 3,-3, 0, 0,
     &  -3, 0, 0,-3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1, 1, 1, 1,-1,
     &   0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,-1, 0, 0, 0, 0, 0, 0,
     &   0, 0, 0,-1,-1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0,-3,-1, 3, 0, 0, 2, 2, 2, 0, 0, 0,-3, 0,-1, 0, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3,-1,-1, 0, 0, 0, 0,
     &   0, 0, 0, 0,-1, 0, 1, 1,-1,-1, 1, 1, 1, 0, 0, 2, 2, 2, 2, 0,
     &   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1,-1,-1,-1, 3, 3, 3, 3,
     &   3, 3, 0, 0, 0, 0, 0, 0, 2, 2, 2, 2, 2, 0, 0, 0, 0, 0, 0, 0,
     &   2, 2, 2, 1, 1, 1, 2, 2, 0, 0, 0, 0, 1,-1,-1, 0, 0, 0, 0, 0,
     &   0,-2, 0,-2, 0,-2, 0,-1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 2, 0, 0,
     &   0, 0, 0, 0, 0,-1,-1, 0, 0, 0, 2, 2, 2, 0, 0, 0, 0, 0, 0, 0,
     &  -3,-3,-3,-3, 0, 0, 0,-2,-2,-2, 0,-3,-3,-3,-3,-2,-2,-2,-2,-2,
     &  -2, 0, 0, 0, 0,-3,-3,-3,-3, 0, 0, 0, 0,-3,-3,-3, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0 /
!        1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
      DATA IUS /
     &  -2,-2,-2, 0, 0,-2,-2,-2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0,-2,-3,-3,-3, 0, 0, 0, 0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-3,
     &  -3,-3, 0,-1,-1,-1,-1,-1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-2,
     &   0, 0, 0, 0, 0, 0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-3,-3,-3, 0, 0,
     &  -2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1,-1,-1,-1,-1,-1,-1,
     &  -1,-1,-1,-1,-1, 0, 0,-2,-2,-2, 0, 0, 0,-1,-1,-1, 0,-2, 0,-1,
     &  -1,-1,-3, 0,-1, 0, 0, 0, 0, 0, 0, 0, 0,-1, 0,-1, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0, 0, 0, 0,-1, 0, 0, 0, 0, 0,-1, 0, 0, 0,-2,
     &   0, 0, 0,-1,-1,-1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0, 0,-3,-1, 0, 0, 0, 0,-2,-1,-1,-1,-1, 0, 0,
     &   0, 0, 0,-2,-2,-2, 0,-3,-3,-3,-3,-3,-1, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0, 1,-1,-1,-1, 0, 0, 0, 0, 0, 0, 0, 0,-2, 0, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1,-2,-2, 0, 0, 0, 0,
     &   0, 0, 0, 0,-2, 0, 0, 0,-1,-1, 0, 0, 0, 0, 0,-2,-2,-2,-1, 0,
     &   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-2,-2,-2,-2,-1,-1,-1,-1,
     &  -1,-1,-1,-1,-1,-1,-1,-1,-3,-3,-3,-3,-3, 0, 0, 0, 0, 0, 0, 0,
     &  -2,-2,-2,-3,-3,-3,-1,-1,-1, 0,-1, 0, 0,-2,-2, 0, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0, 0,-2, 0, 0, 0, 0, 0, 0, 0, 0, 0,-2, 0, 0,
     &   0, 0, 0, 0, 0,-2,-2, 0, 0, 0, 0, 0, 0, 0, 0,-2, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0, 0,-1,-1,-1, 0, 0, 0, 0, 0,-1,-1,-1,-1,-1,
     &  -1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1, 0, 0,
     &   0, 0, 0, 0, 0, 0 /
!        1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
      DATA IUKG /
     &   1, 1, 1, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1,
     &   1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
     &   1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 1,
     &   0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0,
     &   1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1,
     &   1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1,
     &   1, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0,
     &   1, 0, 0, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1,
     &   0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0,
     &   0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0,
     &   0, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1,
     &   0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1,
     &   1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1,
     &   1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0,
     &   0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0, 0, 1, 1, 1, 0, 0, 1, 1, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0,
     &   1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
     &   0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0, 0, 0, 0,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0 /
!        1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
      DATA IUK /
     &   0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0, 0,-1,-1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0,
     &   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
     &   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0, 0,-1,-1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0 /
!        1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
      DATA IUMOL /
     &   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
     &   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0,
     &   0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0,
     &   0, 0, 0, 0, 0, 0 /
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RDOUUN'
      IF( INDEX(SVN_ID(156)(1:1),'$').EQ.0 ) SVN_ID(156) =
     & '$Id: rdouun.F 1080 2017-03-14 16:22:02Z d3c002 $' 
      JNDX = INDX
      KNDX = 400+NSOLU*33
      IF( INDX.GT.KNDX ) THEN
        IF( MOD((INDX-KNDX),33).EQ.0 ) THEN
          JNDX = 466
        ELSE
          JNDX = MOD((INDX-KNDX),33)+433
        ENDIF
      ELSEIF( INDX.GT.400 ) THEN
        IF( MOD((INDX-400),33).EQ.0 ) THEN
          JNDX = 433
        ELSE
          JNDX = MOD((INDX-400),33)+400
        ENDIF
      ENDIF
      IUNM = IUM(JNDX)
      IUNKG = IUKG(JNDX)
      IUNS = IUS(JNDX)
      IUNMOL = IUMOL(JNDX)
      IUNK = IUK(JNDX)
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RDOUUN group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDSFUN( INDX )
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
!     Read surface flux rate and integral units.
!     Positive INDX indicates rate units.
!     Negative INDX indicates integral units.
!
!  1    Heat Flux
!  2    Aqueous Volumetric Flux
!  3    Gas Volumetric Flux
!  4    NAPL Volumetric Flux
!  5    Aqueous Mass Flux
!  6    Gas Mass Flux
!  7    NAPL Mass Flux
!  8    Salt Mass Flux
!  9    Aqueous Oil Mass Flux
!  11   Condensate Water Mass Flux
!  11   Gas CH4 Mass Flux
!  12   Aqueous CH4 Mass Flux
!  13   CH4 Mass Flux
!  20   Gas-Advective Heat Flux
!  21   Gas-Advective Water-Mass Flux
!  22   Gas-Advective Air-Mass Flux
!  25   Gas-Diffusive Heat Flux
!  26   Gas-Diffusive Water-Mass Flux
!  27   Gas-Diffusive Air-Mass Flux
!  28   Gas CO2 Mass Flux
!  29   Aqueous CO2 Mass Flux
!  30   Total CO2 Mass Flux
!  31   Gas-Advective Oil-Mass Flux
!  32   Gas-Diffusive Oil-Mass Flux
!  33   Gas-Total Oil-Mass Flux
!  34   Actual Evaporation
!  35   Potential Evaporation
!  36   Actual Transpiration
!  37   Potential Transpiration
!  38   Net Radiation
!  39   Net Short-Wave Radiation
!  40   Net Long-Wave Radiation
!  41   surface water-mass balance
!  42   surface rain-water runoff
!  43   Aqueous Water Mass Flux
!  44   Gas Water Mass Flux
!  45   Total Water Mass Flux
!  46   Aqueous-advective gas-component mass flux
!  47   Aqueous-diffusive gas-component mass flux
!  48   Gas-advective gas-component mass flux
!  49   Gas-diffusive gas-component mass flux
!  50   total-advective-diffusive gas-component mass flux
!  51   total-diffusive gas-component mass flux
!  52   napl-advective gas-component mass flux
!  53   napl-diffusive gas-component mass flux
!  54   total-advective-diffusive gas-component mass flux
!  55   nonaqueous-liquid CO2 mass flux - UNAM, VNAM, WNAM
!  56   nonaqueous-liquid CH4 mass flux - UNOM, VNOM, WNOM
!  57   nonaqueous-liquid water mass flux - UNWM, VNWM, WNWM
!  58   total N2 mass flux - UN2M, VN2M, WN2M
!  59   aqueous N2 mass flux - ULNM, VLNM, WLNM
!  60   gas N2 mass flux - UGNM, VGNM, WGNM
!  61   nonaqueous-liquid N2 mass flux - UNNM, VNNM, WNNM
!  62   gas component mass flux - UGC, VGC, WGC
!  63   aqueous component mass flux - ULC, VLC, WLC
!  64   nonaqueous-liquid component mass flux - UNC, VNC, WNC
!  65   total component mass flux - UTC, VTC, WTC
!  >100 Solute Flux
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNL, November, 1995.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE TRNSPT
      USE SOLTN
      USE REACT
!

!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Parameter Statements----------------------------!
!



      PARAMETER (LUNS=102)
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      INTEGER IUM(LUNS),IUKG(LUNS),IUS(LUNS),IUK(LUNS),IUMOL(LUNS)
      SAVE IUM,IUKG,IUS,IUK,IUMOL
!
!----------------------Data Statements---------------------------------!
!
!        1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
      DATA IUM /
     &   2, 3, 3, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2,
     &   0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 2, 2,
     &   0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0 /
!        1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
      DATA IUS /
     &  -3,-1,-1,-1,-1,-1,-1,-1,-1, 0,-1,-1,-1, 0, 0, 0, 0, 0, 0,-3,
     &  -1,-1, 0, 0,-3,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-3,-3,-3,
     &   0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     &  -1,-1,-1,-1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   -1,-1 /
!        1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
      DATA IUKG /
     &   1, 0, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1,
     &   1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
     &   0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
     &   1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0 /
!        1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
      DATA IUK /
     &   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0 /
!        1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
      DATA IUMOL /
     &   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &   0, 1 /
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RDSFUN'
      IF( INDEX(SVN_ID(156)(1:1),'$').EQ.0 ) SVN_ID(156) =
     & '$Id: rdouun.F 1080 2017-03-14 16:22:02Z d3c002 $' 
      JNDX = ABS(INDX)
!
!---  Solute surface flux units  ---
!
      IF( JNDX.GT.100 .AND. JNDX.LE.(100+NSOLU) ) THEN
        JNDX = 101
!
!---  Conservation-component species or kinetic-component species
!     surface flux units  ---
!
      ELSEIF( JNDX.GT.(100+NSOLU) .AND. 
     &  JNDX.LE.(100+NSOLU+NEQC+NEQK) ) THEN
        JNDX = 102
      ENDIF
      IUNM = IUM(JNDX)
      IUNKG = IUKG(JNDX)
      IUNS = IUS(JNDX)
      IF( INDX.LT.0 ) IUNS = IUNS+1
      IUNMOL = IUMOL(JNDX)
      IUNK = IUK(JNDX)
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RDSFUN group
!
      RETURN
      END

