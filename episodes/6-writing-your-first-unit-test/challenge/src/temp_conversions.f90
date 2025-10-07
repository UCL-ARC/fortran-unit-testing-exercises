module temp_conversions
    implicit none
    private
    public :: fahrenheit_to_celsius, celsius_to_kelvin

contains

    function fahrenheit_to_celsius(fahrenheit) result(celsius)
        real, intent(in) :: fahrenheit
        real :: celsius
        celsius = (fahrenheit - 32.0) * 5.0 / 9.0
    end function fahrenheit_to_celsius

    function celsius_to_kelvin(celsius) result(kelvin)
        real, intent(in) :: celsius
        real :: kelvin
        kelvin = celsius + 273.15
    end function celsius_to_kelvin

end module temp_conversions