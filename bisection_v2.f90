! Método de la bisección para diferentes funciones
! Autor: Danilo toro

module ExampleFuncs

   implicit none

contains
    function quadratic(a,b,c,d,x)
    implicit none
        real quadratic, a, b, c, d, x
        quadratic = a*x**2.0 + b*x + c
        return
    end function quadratic

    function lineal(m,n,c,d,x)
    implicit none
        real lineal, m, n, c, d, x
        lineal = m*x + n
        return
    end function lineal

    function cubic(a, b, c, d, x)
    implicit none
        real cubic, a, b, c, d, x
        cubic = a*x**3.0 + b*x**2.0 + c*x + d
        return
    end function cubic

    function square(a, b,c,d,x)
    implicit none
        real square, a, b, c, d, x
        if((b*x + c).ge.0.0) then
            square = a*sqrt(b*x + c)
        end if
        return
    end function square

    function coseno(a, n, c,d, x)
    implicit none
        real coseno, n, a, c, d, x
        coseno = n* cos(a*x + c)
        return
    end function coseno

    function seno(a, n, c,d, x)
    implicit none
        real seno, n, a, c, d, x
        seno = n*sin(a*x + c)
        return
    end function seno

    function logn(a, n, c,d, x)
    implicit none
        real logn, n, a, c, d, x
        if((a*x + c).gt.0.0) then
            logn = n* log(a*x + c)
        end if
        return
    end function logn

    function logarithmic(a,n, c, d, x)
    implicit none
        real logarithmic, n, a, c, d, x
        if((a*x + c).gt.0.0) then
            logarithmic = n* log10(a*x + c)
        end if
        return
    end function logarithmic
end module ExampleFuncs


program bisectionv2

    use ExampleFuncs

    !Root finding program
    implicit none

    ! Variables
    integer option
    real a, b, c, d ! a,b,c,d coefficients
    real upper_lim, lower_lim, root
    real tolerance, fun

    abstract interface
      function func (a,b,c,d,z)
         real :: func
         real a,b,c,d, z
      end function func
   end interface

   procedure (func), pointer :: f_ptr => null ()

    write(*,*) "What function do you want to find the root of?"
    write(*,*) "1. Lineal -- f(x) = mx + n"
    write(*,*) "2. Quadratic -- f(x) = a x^2 + b x + c"
    write(*,*) "3. Cubic -- f(x) = a x^3 + b x^2 + c x + d"
    write(*,*) "4. Square root -- f(x) = a sqrt(bx+ c)"
    write(*,*) "5. Sinusoid -- f(x) = n cos(ax + c)"
    write(*,*) "6. Sinusoid -- f(x) = n sen(ax + c)"
    write(*,*) "7. logarithmic -- f(x) = n log10(ax + c)"
    write(*,*) "8. logarithmic -- f(x) = n ln(ax + c)"

    read(*,*) option

    write(*,*) "Enter lower and upper limit. Separate by commas"
    read(*,*) lower_lim, upper_lim

    write(*,*) "Enter error tolerance"
    read(*,*) tolerance

    write(*,*) "Enter coefficients"
    
    if(option.eq.1) then
        read(*,*) a,b
        f_ptr => lineal
    else if(option.eq.2) then
        f_ptr => quadratic
        read(*,*) a,b,c
    else if(option.eq.3) then
        f_ptr => cubic
        read(*,*) a,b,c,d
    else if(option.eq.4) then
        f_ptr => square
        read(*,*) a,b, c
    else if(option.eq.5) then
        f_ptr => coseno
        read(*,*) a,b,c
    else if(option.eq.6) then
        f_ptr => seno
        read(*,*) a,b,c
    else if(option.eq.7) then
        f_ptr => logarithmic
        read(*,*) a,b,c
    else if(option.eq.8) then
        f_ptr => logn
        read(*,*) a,b,c
    end if        

    call bisection(root, tolerance, upper_lim, lower_lim, f_ptr, a, b, c, d)
    
    write(*,*) "Root is: ", root
    
end program bisectionv2

subroutine bisection(root, tolerance, upper_lim, lower_lim, fun, a,b,c,d)
    real upper_lim, lower_lim, mean, x_old
    real error, lower, upper, tolerance
    real root, fun, a, b, c, d
    x_old = lower_lim
    error = 100

    do while(error.ge.tolerance)
        
        mean = (lower_lim + upper_lim) / 2.0
        
        upper = fun(a,b,c,d, mean)
        lower = fun(a,b,c,d, lower_lim)
        write(*,*) upper, mean, error

        ! EVALUAMOS LAS CONDICIONES
        
        if(upper*lower.lt.0.0) then
            upper_lim = mean
        else if(upper*lower.gt.0.0) then
            lower_lim = mean
        else
            exit
        end if

        if(mean.ne.0.0) then
            error = abs((x_old - mean)*100/mean)
        end if

        x_old = mean     
        
    end do
    root = mean
end subroutine bisection




