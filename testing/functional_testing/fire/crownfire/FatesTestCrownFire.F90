program FatesTestCrownFire
  
  use FatesConstantsMod,           only : r8 => fates_r8 
  use FatesArgumentUtils,          only : command_line_arg
  use FatesUnitTestParamReaderMod, only : fates_unit_test_param_reader

  implicit none

  ! LOCALS:
  type(fates_unit_test_param_reader) :: param_reader                 ! param reader instance
  character(len=:), allocatable      :: param_file                   ! input parameter file
  real(r8),         allocatable      :: CWC(:)                       ! canopy water content [%]
  real(r8),         allocatable      :: CBD(:)                       ! canopy bulk density in biomass [kg/m3]
  real(r8),         allocatable      :: CBH(:)                       ! canopy base height [m]
  !real(r8),         allocatable      :: SAV(:)                       ! fuel surface area to volume ratio [/cm]
  real(r8),         allocatable      :: smp(:)                       ! soil matric potential [MPa]
  real(r8),         allocatable      :: smp_alpha(:)                 ! coefficient associate with smp for LFMC [unitless]
  real(r8),         allocatable      :: wind(:)                      ! wind speed [km/hr]
  real(r8),         allocatable      :: passive_crown_fi(:,:)        ! min surface fire intensity to ignite crown fuel [kW/m]
  real(r8),         allocatable      :: CI_FM10(:)                   ! open wind speed at which a fully active crown fire is maintained using fule model 10 [km/hr]
  real(r8),         allocatable      :: LFMC(:,:)                    ! live fuel moisture content [%]
  real(r8),         allocatable      :: ROS_active_FM10(:,:)         ! active crown fire spread rate using fuel model 10 [m/min] 

  ! CONSTANTS:
  character(len=*), parameter :: out_file = 'crownfire_out.nc' ! output file 
  real(r8),         parameter :: lai = 3.0_r8                  ! leaf area index (for LFMC) [m2/m2]
  real(r8),         parameter :: lai_beta = 0.15_r8            ! coefficient associate with LAI (for LFMC) [unitless]
  real(r8),         parameter :: min_lfmc = 50.0_r8            ! min live fuel moisture (for LFMC) [%]
  real(r8),         parameter :: coeff_lfmc = 30.0_r8          ! value adds to min_lfmc in response to change in smp and lai (for LFMC) [unitless]
  real(r8),         parameter :: gamma_int = 0.0_r8            ! model coef. for interaction effect of swc and lai on LFMC [unitless]
  
  interface

    subroutine TestPassCrownFI(CBH, CWC, passive_crown_fi)

        use FatesConstantsMod,        only : r8 => fates_r8 
        use CrownFireEquationsMod,    only : PassiveCrownFireIntensity
        implicit none
        real(r8), allocatable, intent(out)   :: CBH(:)
        real(r8), allocatable, intent(out)   :: CWC(:)
        real(r8), allocatable, intent(out)   :: passive_crown_fi(:,:)

    end subroutine TestPassCrownFI

    subroutine TestLiveFuelMoisture(smp, smp_alpha, LFMC)

        use FatesConstantsMod,        only : r8 => fates_r8 
        use CrownFireEquationsMod,    only : LiveFuelMoistureContent
        implicit none
        real(r8), allocatable, intent(out)   :: smp(:)
        real(r8), allocatable, intent(out)   :: smp_alpha(:)
        real(r8), allocatable, intent(out)   :: LFMC(:,:)

    end subroutine TestLiveFuelMoisture

    subroutine TestCrownFireFM10(CBD, wind, ROS_acitive_FM10, CI_FM10)

        use FatesConstantsMod,        only : r8 => fates_r8 
        use CrownFireEquationsMod,    only : CrownFireBehaveFM10
        implicit none
        real(r8), allocatable, intent(out)   :: CBD(:)
        real(r8), allocatable, intent(out)   :: wind(:)
        real(r8), allocatable, intent(out)   :: ROS_acitive_FM10(:,:)
        real(r8), allocatable, intent(out)   :: CI_FM10(:)
    
    end subroutine TestCrownFireFM10

    subroutine WriteCrownFireData(out_file, CBH, CWC, passive_crown_fi, &
        smp, smp_alpha, LFMC, CBD, wind, ROS_acitive_FM10, CI_FM10)

        use FatesConstantsMod, only : r8 => fates_r8
        use FatesUnitTestIOMod,  only : OpenNCFile, CloseNCFile, RegisterNCDims
        use FatesUnitTestIOMod,  only : RegisterVar, EndNCDef, WriteVar
        use FatesUnitTestIOMod,  only : type_double
        implicit none
        character(len=*), intent(in) :: out_file
        real(r8),             intent(in)  :: CBH(:)
        real(r8),             intent(in)  :: CWC(:)
        real(r8),             intent(in)  :: passive_crown_fi(:,:)
        real(r8),             intent(in)  :: smp(:)
        real(r8),             intent(in)  :: smp_alpha(:)
        real(r8),             intent(in)  :: LFMC(:,:)
        real(r8),             intent(in)  :: CBD(:)
        real(r8),             intent(in)  :: wind(:)
        real(r8),             intent(in)  :: ROS_active_FM10(:,:)
        real(r8),             intent(in)  :: CI_FM10(:)

    end subroutine WriteCrownFireData
  
  end interface

  ! read in parameter file name
  param_file = command_line_arg(1)

  ! read in parameter file
  call param_reader%Init(param_file)
  call param_reader%RetrieveParameters()

  ! calculate minimum fire intensity for igniting crown fuels
  call TestPassCrownFI(CBH, CWC, passive_crown_fi)

  ! calculate live fuel moisture content
  call TestLiveFuelMoisture(smp, smp_alpha, LFMC)

  ! calculate active crown fire spread rate and crowning index using fuel model 10
  call TestCrownFireFM10(CBD, wind, ROS_active_FM10, CI_FM10)

  ! write out data
  call WriteCrownFireData(out_file, CBH, CWC, passive_crown_fi, &
        smp, smp_alpha, LFMC, CBD, wind, ROS_acitive_FM10, CI_FM10)

  ! deallocate arrays
  if (allocated(CBH)) deallocate(CBH)
  if(allocated(CWC)) deallocate(CWC)
  if(allocated(passive_crown_fi)) deallocate(passive_crown_fi)
  if(allocated(smp)) deallocate(smp)
  if(allocated(smp_alpha)) deallocate(smp_alpha)
  if(allocated(LFMC)) deallocate(LFMC)
  if(allocated(CBD)) deallocate(CBD)
  if(allocated(wind)) deallocate(wind)
  if(allocated(ROS_acitive_FM10)) deallocate(ROS_acitive_FM10)
  if(allocated(CI_FM10)) deallocate(CI_FM10)

end program FatesTestCrownFire

!=========================================================================================

subroutine TestPassCrownFI(CBH, CWC, passive_crown_fi)
  !
  ! DESCRIPTION:
  ! Calculate min surface fire intensity to ignite crown fuel over a range of
  ! canopy base height and canopy water content
  !
  use FatesConstantsMod,       only : r8 => fates_r8 
  use CrownFireEquationsMod,   only : PassiveCrownFireIntensity

  implicit none

  ! ARGUMENTS:
  real(r8), allocatable, intent(out) :: CBH(:)                 ! canopy base height [m]
  real(r8), allocatable, intent(out) :: CWC(:)                 ! canopy water content [%]
  real(r8), allocatable, intent(out) :: passive_crown_fi(:,:)  ! min energy to ignite crown fuels [kW/m]

  ! CONSTANTS:
  real(r8), parameter                :: CBH_min = 0.0_r8       ! min canopy base height [m]
  real(r8), parameter                :: CBH_max = 10.0_r8      ! max canopy base height [m]
  real(r8), parameter                :: CBH_inc = 1.0_r8       ! CBH increment to scale [m]
  real(r8), parameter, dimension(6)  :: canopy_water_content = (/10.0_r8, 30.0_r8, 50.0_r8, 70.0_r8, 90.0_r8, 110.0_r8/) ! CWC to use

  !LOCALS:
  integer :: num_CBH ! size of CBH arrays
  integer :: i, j    ! looping indices

  ! allocate arrays
  num_CBH = int((CBH_max - CBH_min) / CBH_inc +1)
  allocate(passive_crown_fi(num_CBH, size(canopy_water_content)))
  allocate(CBH(num_CBH))
  allocate(CWC(size(canopy_water_content)))

  do i = 1, num_CBH
  
    CBH(i) = CBH_min + CBH_inc*(i-1)
    
    do j = 1, size(canopy_water_content)
        CWC(j) = canopy_water_content(j)
        passive_crown_fi(i, j) = PassiveCrownFireIntensity(CBH(i), CWC(j))
    end do
  end do

end subroutine TestPassCrownFI

!=========================================================================================

subroutine TestLiveFuelMoisture(smp, smp_alpha, LFMC)
  !
  ! DESCRIPTION:
  ! Calculate live fuel moisture content over a range of soil matirc potential and associated coefficient
  !
  
  use FatesConstantsMod,       only : r8 => fates_r8 
  use CrownFireEquationsMod,   only : LiveFuelMoistureContent

  implicit none

  ! ARGUMENTS:
  real(r8), allocatable, intent(out) :: smp(:)              ! soil matric potential [MPa]
  real(r8), allocatable, intent(out) :: smp_alpha(:)        ! coefficient associate with smp for predicting LFMC [unitless]
  real(r8), allocatable, intent(out) :: LFMC(:,:)           ! live fule moisture content [%]

  ! CONSTANTS:
  real(r8), parameter                :: smp_max = 0.0_r8       ! max soil matric potential [MPa]
  real(r8), parameter                :: smp_min = -10.0_r8     ! min soil matric potential [MPa]
  real(r8), parameter                :: smp_inc = -1.0_r8      ! smp increment to scale    [MPa]
  real(r8), parameter, dimension(5)  :: smp_coef = (/1E-6_r8, 3E-6_r8, 5E-6_r8, 7E-6_r8, 1E-5_r8/)   ! model coeff associated with smp to use
  real(r8), parameter                :: lai = 3.0_r8           ! leaf area index for LFMC  [m2/m2]
  real(r8), parameter                :: min_lfmc = 50.0_r8     ! min LFMC [%]
  real(r8), parameter                :: coef_lfmc = 30.0_r8    ! value add to min LFMC as a response to change in smp and lai [unitless]
  real(r8), parameter                :: lai_beta = 0.15_r8     ! coefficient associate with lai for LFMC [unitless]
  real(r8), parameter                :: gamma_int = 0.0_r8     ! coefficient associate with the LAI and SMP interaction term

  ! LOCALS:
  integer    :: num_smp    ! size of soil matric potential
  integer    :: i, j      ! looping indicies

  ! allocate arrays
  num_smp = int((smp_max - smp_min) / smp_inc + 1)
  allocate(smp(num_smp))
  allocate(smp_alpha(size(smp_coef)))
  allocate(LFMC(num_smp, size(smp_coef)))

  do i = 1, num_smp

    smp(i) = smp_max + smp_inc*(i-1)

    do j = 1, size(smp_coef)
        smp_alpha(j) = smp_coef(j)
        LFMC(i, j) = LiveFuelMoistureContent(lai, smp(i), min_lfmc, coef_lfmc, &
        smp_alpha(j), lai_beta, gamma_int)
    end do
  end do

end subroutine TestLiveFuelMoisture

!=========================================================================================

subroutine TestCrownFireFM10(CBD, wind, ROS_acitive_FM10, CI_FM10)
  !
  ! DESCRIPTION:
  ! Calculate fully active crown fire spread rate using fule model 10 over a range of CBD and wind speed
  ! and open wind speed to initiate crown fire over a range of CBD assuming fule model 10
  !

  use FatesConstantsMod,      only : r8 => fates_r8 
  use CrownFireEquationsMod,  only : CrownFireBehaveFM10

  implicit none

  ! ARGUMENTS:
  real(r8), allocatable, intent(out) :: CBD(:)                  ! canopy bulk density in biomass [kg/m3]
  real(r8), allocatable, intent(out) :: wind(:)                 ! wind speed [km/hr]
  real(r8), allocatable, intent(out) :: ROS_acitive_FM10(:,:)   ! fully active crown fire spread rate using fule model 10
  real(r8), allocatable, intent(out) :: CI_FM10(:)              ! crowning index using fule model 10 [km/hr]

  ! CONSTANTS:
  real(r8), parameter                :: CBD_min = 0.0_r8        ! min canopy bulk density [kg/m3]
  real(r8), parameter                :: CBD_max = 0.5_r8        ! max canopy bulk density [kg/m3]
  real(r8), parameter                :: CBD_inc = 0.01_r8       ! CBD increment to scale [unitless]
  real(r8), parameter, dimension(6)  :: wind_vals = (/5.0_r8, 10.0_r8, 15.0_r8, 25.0_r8, 35.0_r8, 45.0_r8/)
  real(r8), parameter                :: fire_weather_index = 5000_r8 ! Nesterove fire weather index [unitless]
  real(r8), parameter.               :: kmhr_to_mmin = 16.6667_r8  ! convert km/hour to m/min for wind speed
  
  ! LOCALS:
  integer            :: num_CBD     ! size of canopy bulk density array
  integer            :: i, j        ! looping indicies
  real(r8)           :: ROS_active  ! ROS_active output from CrownFireBehaveFM10 [m/min]
  real(r8)           :: CI          ! CI output from CrownFireBehaveFM10 [m/min]

  ! allocate arrays
  num_CBD = int((CBD_max - CBD_min) / CBD_inc + 1)
  allocate(CBD(num_CBD))
  allocate(wind(size(wind_vals)))
  allocate(ROS_acitive_FM10(num_CBD, size(wind_vals)))
  allocate(CI_FM10(num_CBD))

  do i = 1, num_CBD
    CBD(i) = CBD_min + CBD_inc * (i-1)

    do j = 1, size(wind_vals)
        wind(j) = wind_vals(j) * kmhr_to_mmin  ! convert wind speed to m/min
        call CrownFireBehaveFM10(SF_val_drying_ratio, fire_weather_index, SF_val_miner_total, &
        SF_val_part_dens, wind(j), CBD(i), ROS_active, CI)
        ROS_active_FM10(i, j) = ROS_active
        CI_FM10(i) = CI
    end do
  end do

end subroutine TestCrownFireFM10

!=========================================================================================

subroutine WriteCrownFireData(out_file, CBH, CWC, passive_crown_fi, &
        smp, smp_alpha, LFMC, CBD, wind, ROS_acitive_FM10, CI_FM10)
  !
  ! DESCRIPTION:
  ! write out data from the test
  !
  use FatesConstantsMod, only : r8 => fates_r8
  use FatesUnitTestIOMod,  only : OpenNCFile, CloseNCFile, RegisterNCDims
  use FatesUnitTestIOMod,  only : RegisterVar, EndNCDef, WriteVar
  use FatesUnitTestIOMod,  only : type_double

  implicit none

  ! ARGUMENTS:
  character(len=*), intent(in) :: out_file
  real(r8),             intent(in)  :: CBH(:)
  real(r8),             intent(in)  :: CWC(:)
  real(r8),             intent(in)  :: passive_crown_fi(:,:)
  real(r8),             intent(in)  :: smp(:)
  real(r8),             intent(in)  :: smp_alpha(:)
  real(r8),             intent(in)  :: LFMC(:,:)
  real(r8),             intent(in)  :: CBD(:)
  real(r8),             intent(in)  :: wind(:)
  real(r8),             intent(in)  :: ROS_active_FM10(:,:)
  real(r8),             intent(in)  :: CI_FM10(:)

  ! LOCALS:
  integer           :: ncid         ! netcdf id
  character(len=20) :: dim_names(6) ! dimension names
  integer           :: dimIDs(6)    ! dimension IDs
  integer           :: CBHID
  integer           :: CWCID
  integer           :: pasv_crwn_ID
  integer           :: smpID
  integer           :: smp_alpha_ID
  integer           :: LFMCID
  integer           :: CBDID
  integer           :: windID
  integer           :: ROSACT_FM10_ID
  integer           :: CI_FM10_ID

  ! dimension names
  dim_names = [character(len=20) :: 'CBH', 'CWC', 'smp', 'smp_alpha', 'CBD', 'wind']

  ! open file
  call OpenNCFile(trim(out_file), ncid, 'readwrite')

  ! register dimensions
  call RegisterNCDims(ncid, dim_names, (/size(CBH), size(CWC), size(smp),        &
    size(smp_alpha), size(CBD), size(wind)/), 6, dimIDs)

  ! first register dimension variables
    ! register CBH 
  call RegisterVar(ncid, 'CBH', dimIDs(1:1), type_double,   &
    [character(len=20)  :: 'units', 'long_name'],           &
    [character(len=150) :: 'm', 'canopy base height'], 2, CBHID)
    
  ! register canopy water content
  call RegisterVar(ncid, 'CWC', dimIDs(2:2), type_double,  &
    [character(len=20)  :: 'units', 'long_name'],                    &
    [character(len=150) :: '%', 'canopy water content'], 2, CWCID)
    
  ! register soil matric potential  
  call RegisterVar(ncid, 'smp', dimIDs(3:3), type_double,   &
    [character(len=20)  :: 'units', 'long_name'],                &
    [character(len=150) :: 'MPa', 'soil matric potential'], 2, smpID)
    
  ! register LFMC model coeff associated with smp
  call RegisterVar(ncid, 'smp_alpha', dimIDs(4:4), type_double,     &
    [character(len=20)  :: 'units', 'long_name'],                    &
    [character(len=150) :: '', 'live fuel moisture model coefficient for soil matric potential'], 2, smp_alpha_ID)
    
  ! register canopy bulk density
  call RegisterVar(ncid, 'CBD', dimIDs(5:5), type_double,     &
    [character(len=20)  :: 'units', 'long_name'],                    &
    [character(len=150) :: 'kg/m3', 'canopy bulk density in biomass'], 2, CBDID)

  ! register wind speed
  call RegisterVar(ncid, 'wind', dimIDs(6:6), type_double,    &
  [character(len=20)  :: 'units', 'long_name'],               &
  [character(len=150) :: 'm/min', 'open wind speed'], 2, windID)

  ! then register actual variables

  ! register min surface fire intensity for igniting crown fuels
  call RegisterVar(ncid, 'passive_crown_fi', dimIDs(1:2), type_double,         &
    [character(len=20)  :: 'coordinates', 'units', 'long_name'],        &
    [character(len=150) :: 'CBH CWC', 'kW/m', 'min fire intensity to ignite crown fuels'],  &
    3, pasv_crwn_ID)
    
  ! register live fuel moisture content 
  call RegisterVar(ncid, 'LFMC', (dimIDs(3:4)), type_double,  &
    [character(len=20)  :: 'coordinates', 'units', 'long_name'],         &
    [character(len=150) :: 'smp smp_alpha', '%', 'live fuel moisture content'], &
    3, LFMCID)
    
  ! register active crown fire spread rate using fuel model 10
  call RegisterVar(ncid, 'ROSACT_FM10', dimIDs(5:6), type_double,  &
    [character(len=20)  :: 'coordinates', 'units', 'long_name'],         &
    [character(len=150) :: 'CBD wind', 'm/min', 'active crown fire ROS using fuel model 10'], &
    3, ROSACT_FM10_ID)
    
  ! register crowning index
  call RegisterVar(ncid, 'CI_FM10', dimIDs(5:5), type_double,  &
    [character(len=20)  :: 'coordinates', 'units', 'long_name'],         &
    [character(len=150) :: 'CBD', 'm/min', 'wind speed at which a active crown fire is sustained, assuming fuel model 10'], &
    3, CI_FM10_ID)
    
    
  ! finish defining variables
  call EndNCDef(ncid)

  ! write out data
  call WriteVar(ncid, CBHID, CBH(:))
  call WriteVar(ncid, CWCID, CWC(:))
  call WriteVar(ncid, pasv_crwn_ID, passive_crown_fi(:,:))
  call WriteVar(ncid, smpID, smp(:))
  call WriteVar(ncid, smp_alpha_ID, smp_alpha(:))
  call WriteVar(ncid, LFMCID, LFMC(:,:))
  call WriteVar(ncid, CBDID, CBD(:))
  call WriteVar(ncid, windID, wind(:))
  call WriteVar(ncid, ROSACT_FM10_ID, ROS_acitive_FM10(:,:))
  call WriteVar(ncid, CI_FM10_ID, CI_FM10(:))
  
  ! close file
  call CloseNCFile(ncid)


end subroutine WriteCrownFireData