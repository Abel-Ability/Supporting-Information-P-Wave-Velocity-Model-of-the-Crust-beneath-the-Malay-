C- SET EARTHQUAKE BORDERS -----------------------------    
      PARAMETER (ELAMIN=  -2.51)   ! MIN LAT (DEGREES)
      PARAMETER (ELAMAX=   8.01)   ! MAX LAT (DEGREES)
      PARAMETER (ELOMIN=  96.51)   ! MIN LON (DEGREES)
      PARAMETER (ELOMAX= 107.51)   ! MAX LON (DEGREES)
      PARAMETER (DEPMIN=   0.00)   ! MIN DEP (KM)
      PARAMETER (DEPMAX= 100.01)   ! MAX DEP (KM)
      PARAMETER (EMGMIN=   2.51)   ! MIN MAGNITUDE                      
C
C-- SET STATION BORDERS--------------------------------
      PARAMETER (SLAMIN=   0.00)   ! MIN LAT OF STATION
      PARAMETER (SLAMAX=   7.00)   ! MAX LAT OF STATION
      PARAMETER (SLOMIN= 100.00)   ! MIN LON OF STATION
      PARAMETER (SLOMAX= 104.50)   ! MAX LON OF STATION
C
C-- SET HORIZONTAL PLANE OF GRID (MODEL SPACE)------------
      PARAMETER (GLAMIN=   1.00)   ! MIN LAT (DEGREES)
      PARAMETER (GLAMAX=   7.00)   ! MAX LAT (DEGREES)
      PARAMETER (GLOMIN= 100.00)   ! MIN LON (DEGREES)
      PARAMETER (GLOMAX= 104.50)   ! MAX LON (DEGREES)
C
C- SET GRID NODE PLUS ENDPOINTS -----------------------
       PARAMETER (MX=27)            ! LAT GRID POINTS (15 minutes) 
       PARAMETER (MY=21)            ! LON GRID POINTS (15 minutes) 
C      PARAMETER (MX=15)            ! LAT GRID POINTS (30 minutes) 
C      PARAMETER (MY=12)            ! LON GRID POINTS (30 minutes)  
       PARAMETER (MZ=13)            ! DEP GRID POINTS (MZ-2)
C
C-  SET ANOMALY     
       PARAMETER (CYA1=   4.00)   ! CENTRAL LAT (DEG)
       PARAMETER (CYO1= 101.00)   ! CENTRAL LON (DEG)
       PARAMETER (DCY1=   5.00)   ! DEPTH (KM) TO TOP   
       PARAMETER (HCY1=  60.00)   ! HEIGHT (KM)
       PARAMETER (RCY1=  40.00)   !
C
       PARAMETER (CYA2=   2.50)   ! CENTRAL LAT (DEG) 
       PARAMETER (CYO2= 102.00)   ! CENTRAL LON (DEG) 
       PARAMETER (DCY2=   5.00)   ! DEPTH (KM) TO TOP     
       PARAMETER (HCY2=  85.00)   ! HEIGHT (KM)
       PARAMETER (RCY2=  60.00)   !
C
       PARAMETER (CYA3=   1.80)   ! CENTRAL LAT (DEG)
       PARAMETER (CYO3= 103.50)   ! CENTRAL LON (DEG)
       PARAMETER (DCY3=   5.00)   ! DEPTH (KM) TO TOP    
       PARAMETER (HCY3=  45.00)   ! HEIGHT (KM)
       PARAMETER (RCY3=  45.00)   !    
C
C- CONSTANTS ------------------------------------------------
      PARAMETER (PTBM=3.0)         ! PERTURBATION LIMIT
      PARAMETER (STDV=0.1)         
      PARAMETER (MINDAT=3)         
      PARAMETER (MINYR=1964)       
      PARAMETER (MAXYR=2025)       
      PARAMETER (DCOR=1500.0)      
      PARAMETER (MEQ=50000)        
      PARAMETER (MS=100)           
      PARAMETER (ML=25)            
      PARAMETER (MDAT=100000)      
      PARAMETER (XMAX=1200.0)      
      PARAMETER (RESMAX=3.5)       
      PARAMETER (ZAX=5.0)          
      PARAMETER (MSTEP=300)        
      PARAMETER (MG=9000000)       
      PARAMETER (MGD=2500)         
      PARAMETER (RMSCUT=0.25)      
      PARAMETER (STEPL=5.0)        
      PARAMETER (DAMPFAC=20.0)     
      PARAMETER (MINOBS=10)        
      PARAMETER (ITRMAX=1)         
      PARAMETER (LOCMAX=1)         
      PARAMETER (ITEMAX=400)       
      PARAMETER (PTBMAX=0.5)       
      PARAMETER (R0=6371.05)       
      PARAMETER (DKM=111.19)       
      PARAMETER (DEG=0.01745329)   
      PARAMETER (D90=1.5707961)    
      PARAMETER (PI=3.14159265)    
      PARAMETER (EPS=1.0E-6)       
C--- 
      CHARACTER LNE*200            
      CHARACTER LNS*150            
      CHARACTER EVID*9             
      CHARACTER EQID*9             
      CHARACTER CMAG*5             
      CHARACTER ARID*8             
      CHARACTER STA*5              
      CHARACTER STNA*5             
      CHARACTER PHAS*5             
      CHARACTER NTW*5              
      CHARACTER STN*5              
      CHARACTER STUS*7             
      CHARACTER SLOC*21            
      CHARACTER SCTY*30            
      CHARACTER PTBU*32            
      CHARACTER LN1*200            
      CHARACTER LN2*150            
      CHARACTER LNS1*150           
      COMMON/CEAR/ LN1(MEQ)        
      COMMON/CEAR/ NARR(MEQ)       
      COMMON/CEAR/ LN2(MEQ)        
      COMMON/CEAR/ LNS1(200,MEQ)   
C- CONVERT COORDINATES 
      CHARACTER CEW*1             
      CHARACTER CNS*1             
      CHARACTER CLAT*8            
      CHARACTER CLON*9            
      CHARACTER CLAD*2            
      CHARACTER CLAM*2            
      CHARACTER DLAS*2            
      CHARACTER CLAS*5            
      CHARACTER CLOD*3            
      CHARACTER CLOM*2            
      CHARACTER DLOS*2            
      CHARACTER CLOS*5            
      COMMON/CHSR/ CEW,CNS,CLAT,CLAD,CLAM,CLON,CLOD
      COMMON/CHSR/ CLOM,CLAS,CLOS,DLAS,DLOS
C- CONSOLE OPTIONS -----------------------------------------
      COMMON/DDB/ MPS              
      COMMON/DDI/ MGP              
      COMMON/DDA/ PTB              
      COMMON/DDA/ JBODY            
      COMMON/DDG/ JHYPO            
      COMMON/DDD/ JPATH1           
      COMMON/DDE/ JPATH2           
      COMMON/DDF/ JAVRES           
      COMMON/DDJ/ JMODEL           
      COMMON/DDK/ BCY1,BCY2,BCY3   
C--
      COMMON/GRD/ MX1,MY1,MZ1      
      COMMON/GRD/ MX2,MY2,MZ2      
      COMMON/GRD/ MXY              
      COMMON/GRD/ NODES            
C---------------------------------
      COMMON/CRL/ LNE,LNS         
      COMMON/SLN/ NTW             
      COMMON/EVV/ EVID            
      COMMON/EVV/ CMAG            
C- 1D VELOCITY MODEL--------------
      COMMON/V1I/ NL              
      COMMON/V1I/ MOH             
      COMMON/V1D/ BV(ML)          
      COMMON/V1D/ VP(ML)          
      COMMON/V1D/ VS(ML)          
C---
      COMMON/EQX/ ELAT_MIN        
      COMMON/EQX/ ELAT_MAX        
      COMMON/EQX/ ELON_MIN        
      COMMON/EQX/ ELON_MAX        
      COMMON/EQX/ DEP_MIN         
      COMMON/EQX/ DEP_MAX         
      COMMON/EQX/ FMAG_MIN        
      COMMON/EQX/ FMAG_MAX        
C---
      COMMON/STK/ SLAT_MIN        
      COMMON/STK/ SLAT_MAX        
      COMMON/STK/ SLON_MIN        
      COMMON/STK/ SLON_MAX        
      COMMON/STK/ SELV_MIN        
      COMMON/STK/ SELV_MAX        
      COMMON/STK/ DIST_MIN        
      COMMON/STK/ DIST_MAX        
C---
      COMMON/VDX/ GX(MX)          
      COMMON/VDX/ GY(MY)          
      COMMON/VDX/ GZ(MZ)          
      COMMON/V3D/ DV0(MX,MY,MZ)   
      COMMON/V3D/ DV1(MX,MY,MZ)   
      COMMON/V3D/ DV2(MX,MY,MZ)   
      COMMON/V3D/ VE0(MX,MY,MZ)   
      COMMON/V3D/ VE1(MX,MY,MZ)   
      COMMON/V3D/ VE2(MX,MY,MZ)   
      COMMON/V3D/ SER(MX,MY,MZ)   
      COMMON/V3I/ KITP(MX,MY,MZ)  
C- STN 
      COMMON/OBI/ JDAT            
      COMMON/OBI/ JSTN            
      COMMON/CSS/ STN(MS)         
      COMMON /CR/ SLOC(MS)        
      COMMON /CS/ STUS(MS)        
      COMMON /CR/ SCTY(MS)        
      COMMON /CS/ NST             
      COMMON/STQ/ SLAT(MS)        
      COMMON/STQ/ SLON(MS)        
      COMMON/STQ/ SELV(MS)        
      COMMON/STQ/ NSTA            
C----   
      COMMON /CC/ STA             
      COMMON /CC/ PHAS            
      COMMON /CC/ ARID            
      COMMON/OBS/ TIMC            
      COMMON/OBS/ IDPS            
      COMMON/OBS/ DXDC            
      COMMON/OBS/ BZMC            
C---
      COMMON/EVW/ NE              
      COMMON/EVW/ NEQS            
      COMMON/EVQ/ NARV(MEQ)       
      COMMON/EVQ/ LAYS(MEQ)       
      COMMON/EVQ/ EQID(MEQ)       
      COMMON/EVQ/ MINO(MEQ)       
      COMMON/EVQ/ ESEC(MEQ)       
      COMMON/EVQ/ ELAT(MEQ)       
      COMMON/EVQ/ ELON(MEQ)       
      COMMON/EVQ/ EDEP(MEQ)       
      COMMON/EVQ/ EMAG(MEQ)       
      COMMON/EVQ/ DVAR(MEQ)       
      COMMON/EVQ/ VTF(MEQ)        
      COMMON/EVQ/ EVC(3,MEQ)
C---------------------------------
      COMMON/HYPINV/ WTT(MS)      
      COMMON/HYPINV/ RESID(MS)    
      COMMON/HYPINV/ DTVAL(MS,4)  
C---
      COMMON/LOC/ AAX             
      COMMON/LOC/ AAY             
      COMMON/LOC/ AAZ             
      COMMON/LOI/ JAX(MGD)        
      COMMON/LOI/ JAY(MGD)        
      COMMON/LOI/ JAZ(MGD)        
C----
      COMMON/WTH/ WV(8)           
      COMMON/WTI/ KX              
      COMMON/WTI/ KY              
      COMMON/WTI/ KZ              
      COMMON/WTI/ KX1             
      COMMON/WTI/ KY1             
      COMMON/WTI/ KZ1             
C----
      COMMON/MII/ NTOPTS          
      COMMON/MII/ NOD             
      COMMON/MII/ NOU             
      COMMON/MII/ JAXA(MG)        
      COMMON/MII/ JHIT(MDAT)      
      COMMON/MII/ JNDEX(MDAT)     
      COMMON/MII/ JVD(MDAT)       
      COMMON/MII/ NAXA(MDAT)      
      COMMON/MIV/ RESWT(MDAT)     
      COMMON/MIV/ AXA(MG)         
      COMMON/MIV/ RNORM           
      COMMON/MIV/ XNORM           
      COMMON/MPB/ NPTB            
      COMMON/PRI/ PTBU(MDAT)      
C--
      COMMON/ZBZ/ STNA(MDAT)      
      COMMON/OBD/ NDAT            
      COMMON/OBD/ JSTC(MDAT)      
      COMMON/OBD/ RESC(MDAT)      
      COMMON/OBD/ RESR(MDAT)      
      COMMON/OBD/ BAZC(MDAT)      
C
C- BEFORE INVERSION --------------
      COMMON/QBE/ MEQS            
      COMMON/QBE/ MAPS            
      COMMON/QBE/ MPA             
      COMMON/QBE/ MSA             
C
C- AFTER INVERSION ---------------
      COMMON/OBE/ NAPS            
      COMMON/OBE/ NAP             
      COMMON/OBE/ NAS             
C
C- DIRECT WAVE (D)----------------
      COMMON/RADI/ NPD            
      COMMON/RADI/ NSD            
      COMMON/RADI/ JBD            
      COMMON/RADI/ JLD(ML)        
      COMMON/RADI/ KLD(ML)        
      COMMON/RADR/ RFD(8,MSTEP)   
C
C- REFRACTED WAVE (R)-------------
      COMMON/RA0I/ NP0            
      COMMON/RA0I/ NS0            
      COMMON/RA0I/ JB0            
      COMMON/RA0I/ JL0(ML)        
      COMMON/RA0I/ KL0(ML)        
      COMMON/RA0R/ RF0(8,MSTEP)   
C
C-  MINIMUM TIME -----------------
      COMMON/RAYI/ NPT            
      COMMON/RAYI/ JSG            
      COMMON/RAYI/ LBD            
      COMMON/RAYI/ JL(ML)         
      COMMON/RAYI/ KL(ML)         
      COMMON/RAYR/ RAY(8,MSTEP)   
      COMMON/RAYR/ TME            

      COMMON/PABO/ P0T0(5),P0T1(5),P0T2(5)
      COMMON/PABO/ P3T0(5),P3T1(5),P3T2(5)
      COMMON/PABO/ P6T0(5),P6T1(5),P6T2(5)
      COMMON/PABO/ S0T0(5),S0T1(5),S0T2(5)
      COMMON/PABO/ S3T0(5),S3T1(5),S3T2(5)
      COMMON/PABO/ S6T0(5),S6T1(5),S6T2(5)
      COMMON/PABO/ DELX(5)

      COMMON/BIDR/ IBD,IB1,IB2,IB3
