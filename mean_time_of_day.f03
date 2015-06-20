! Solution for http://rosettacode.org/wiki/Averages/Mean_time_of_day
program mean_time_of_day
  implicit none

  integer(kind=4), parameter :: dp = kind(0.0d0)

  type time_t
    integer(kind=4) :: hours, minutes, seconds
  end type

  character(len=8), dimension(4), parameter :: times = (/ '23:00:17', &
    '23:40:20', '00:12:45', '00:17:19' /)
  real(kind=dp), dimension(size(times)) :: angles
  integer(kind=4) :: i
  real(kind=dp) :: mean

  do i = lbound(times, 1), ubound(times, 1)
    angles(i) = time_to_angle(str_to_time(times(i)))
  end do

  mean = mean_angle(angles)
  if (mean < 0) mean = 360 + mean

  write(*, fmt='(I2.2, '':'', I2.2, '':'', I2.2)') angle_to_time(mean)
contains
  function mean_angle(angles) result (res)
    real(kind=dp), dimension(:), intent (in) :: angles
    real(kind=dp) :: res

    integer(kind=4) :: n, i
    real(kind=dp) :: x, y

    n = size(angles)
    x = 0
    y = 0
    do i = lbound(angles, 1), ubound(angles, 1)
      x = x + sin(radians(angles(i)))
      y = y + cos(radians(angles(i)))
    end do
    x = x/n
    y = y/n

    res = degrees(atan2(x, y))
  end function

  function radians(angle) result (res)
    real(kind=dp), intent (in) :: angle
    real(kind=dp) :: res

    real(kind=dp), parameter :: pi = 4d0*atan(1d0)

    res = angle/180*pi
  end function

  function degrees(angle) result (res)
    real(kind=dp), intent (in) :: angle
    real(kind=dp) :: res

    real(kind=dp), parameter :: pi = 4d0*atan(1d0)

    res = 180*angle/pi
  end function

  function str_to_time(str, ierr) result (res)
    character(len=*), intent (in) :: str
    integer(kind=4), intent (out), optional :: ierr
    type(time_t) :: res

    ! Assuming time in format hh:mm:ss
    read(str, fmt='(I2, 1X, I2, 1X, I2)') res

    if (present(ierr)) then
      ierr = 0
      if (res%hours < 0 .or. res%hours > 23) ierr = ior(ierr, 1)
      if (res%minutes < 0 .or. res%minutes > 59) ierr = ior(ierr, ishft(1, 1))
      if (res%seconds < 0 .or. res%seconds > 59) ierr = ior(ierr, ishft(1, 2))
    end if
  end function

  function time_to_angle(time) result (res)
    type(time_t), intent (in) :: time
    real(kind=dp) :: res

    real(kind=dp) :: seconds
    real(kind=dp), parameter :: seconds_in_day = 24*60*60

    seconds = time%seconds + 60*time%minutes + 60*60*time%hours

    res = 360*seconds/seconds_in_day
  end function

  function angle_to_time(angle) result (res)
    real(kind=dp), intent (in) :: angle
    type(time_t) :: res

    real(kind=dp) :: seconds
    real(kind=dp), parameter :: seconds_in_day = 24*60*60

    seconds = seconds_in_day*angle/360d0
    res%hours = int(seconds/60d0/60d0)
    seconds = mod(seconds, 60d0*60d0)
    res%minutes = int(seconds/60d0)
    res%seconds = mod(seconds, 60d0)
  end function
end program
