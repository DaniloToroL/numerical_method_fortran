program newton_rapshon
! Método de Newton-Raphson
! Autor: Danilo toro

    implicit none
    ! i, j: contador de iteraciones
    ! max: Número máximo de iteraciones para el método de newton rapshon
    ! l: contador de raíces
    integer i, j, k, max, l
    ! lower, upper: intervalo donde buscará las raíces. +- 20 al rededor de xr
    integer lower, upper
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
    ! roots: Vector con raíces encontradas. Máximo 10 raíces
    real roots(10)
    ! searched: verdadero si la raíz ya había sido encontrada, falso si no
    ! converge: Falso si el método excedió el límite de iteraciones
    logical searched, converge

    ! Pedimos ingresar por consola el valor inicial para comenzar
    write(*,*) "Enter x value"
    read(*,*) xr

    !Pedimos ingresar el error relativo mínimo
    write(*,*) "Enter error tolerance"
    read(*,*) tolerance

    ! Inicializamos nuestras variables
    
    max = 500
    lower = xr - 20
    upper = xr + 20
    l = 0


    ! Mientras estemos por sobre el error mínimo
    do j=lower, upper, 1
        i = 0
        xr = j
        x_old = xr
        error = 100
        do while ( error.ge.tolerance )
            i = i + 1
            converge = .TRUE.
            ! Solo se itera si la derivada no es cero
            if (fx.ne.0.0) then
                fx = deriv(xr)
                xr = xr - (fun(xr) / fx)
            else
                ! Derivative is equal to zero
                ! Pendiente igual a cero
                exit
            end if

            if (i.ge.max) then
                ! Maximum number of iterations
                ! El método supera el número máximo de iteraciones
                converge = .FALSE.
                exit
            elseif (abs(fun(xr)).le.0.00001) then
                ! Near zero function
                exit
            end if

            ! Si la aproximación obtenida no es cero, entonces calculamos el error relativo
            if(xr.ne.0.0) then
                error = abs((x_old - xr)*100/xr)
            end if

            x_old = xr
            
        end do

        ! Buscamos si la raíz encontrada, ya había sido encontrada antes
        ! si fue encontrada, cambiamos la variable a Verdadero
        searched = .FALSE.
        do k = 1, size(roots)
            if( abs(roots(k) - xr).le.0.01 ) then
                searched = .TRUE.
            end if
        end do

        ! Solo si la función converge y no había sido encontrada
        ! (search ~=> converge): 
        if (searched.neqv.converge) then
            roots(l) = xr
            l = l + 1
        end if

        ! Máximo de raices a buscar
        if (l.ge.10) then
            exit
        end if
    end do

    write(*,*) roots
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
