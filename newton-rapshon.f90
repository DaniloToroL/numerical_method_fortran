program newton_rapshon
! Método de Newton-Raphson
! Autor: Danilo toro

    implicit none
    ! i: contador de iteraciones máximas
    integer i, max
    ! xr: aproximación
    ! fx: valor de la derivada evaluada en un punto
    real xr, fx
    ! fun: función
    ! deriv: derivada de la función
    real fun, deriv
    ! x_old: Última aproximación obtenida
    ! error: Error relativo, valor absoluto de (xr - x_old) / xr
    ! tolerance: Error mínimo aceptado para dejar de iterar
    real x_old, error, tolerance

    ! Pedimos ingresar por consola el valor inicial para comenzar
    write(*,*) "Enter x value"
    read(*,*) xr

    !Pedimos ingresar el error relativo mínimo
    write(*,*) "Enter error tolerance"
    read(*,*) tolerance

    ! Inicializamos nuestras variables
    error = 100
    x_old = xr
    max = 50
    i = 0

    ! Mientras estemos por sobre el error mínimo
    do while ( error.ge.tolerance )
        i = i + 1
        ! Solo sigue iterando si la derivada no es cero
        if (fx.ne.0.0) then
            fx = deriv(xr)
            xr = xr - (fun(xr) / fx)
        ! Si la derivada es cero, terminamos el loop
        ! mandamos mensaje por consola
        else
            write(*,*) "Derivative is equal to zero"
            exit
        end if

        ! Si se alcanza el máximo de iteraciones
        if (i.ge.max) then
            write(*,*) "Maximum number of iterations"
            exit
        ! Si la función es lo suficientemente cercana a cero
        elseif (abs(fun(xr)).le.0.001) then
            write(*,*) "Near zero function:", fun(xr), xr 
            exit
        end if

        ! Si la aproximación obtenida no es cero, entonces calculamos el error relativo
        if(xr.ne.0.0) then
            error = abs((x_old - xr)*100/xr)
        end if

        x_old = xr
        
    end do

    write(*,*) "The root", xr, "was found in", i, "iterations"

end program newton_rapshon

function fun(x)
    implicit none
    real fun, x
    fun = x*tan(x) - ((x + 1)/cos(0.1))
    return
end function fun

function deriv(x)
    implicit none
    real deriv, x
    deriv = tan(x) + x/(cos(x)*cos(x)) - 1.00502
    return
end function
