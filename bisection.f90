program bisection

    !Programa de búsqueda de raíces con el método de bisección
    implicit none 

    ! Definimos las variables que se usarán
    ! upper_lim y lower_lim: Límite superior e inferior
    ! mean: Aproximación de la raiz, mediante el promedio
    ! x_old: Última aproximación obtenida
    real upper_lim, lower_lim, mean, x_old
    ! error: Error relativo, valor absoluto de (mean - x_old) / mean
    ! lower y upper: Valor obtenido al reemplazar en la función el límite inferior y superior
    ! tolerance: Error mínimo aceptado para dejar de iterar
    real error, lower, upper, tolerance
    ! quadratic: Nombre de la función (función cuadrática)
    real quadratic


    ! Pedimos ingresar por consola los límites inferior y superior separados por comas
    write(*,*) "Enter lower and upper limit. Separate by commas"
    read(*,*) lower_lim, upper_lim

    !Pedimos ingresar el error relativo mínimo
    write(*,*) "Enter error tolerance"
    read(*,*) tolerance

    ! Tomamos como aproximación inical el valor mínimo. Este cambiará en cada iteración
    x_old = lower_lim
    ! Iniciamos con un error del 100%
    error = 100

    ! Iteramos mientras el error es mayor al límite ingresado
    do while(error.ge.tolerance)
        ! Obtenemos la aproximación de la raiz
        mean = (lower_lim + upper_lim) / 2.0
        ! EVALUAMOS LOS LÍMITES EN LA FUNCIÓN 
        ! Se decidió comenzar desde el límite inferior hasta la mitad del intervalo
        upper = quadratic(mean)
        lower = quadratic(lower_lim)

        ! EVALUAMOS LAS CONDICIONES
        ! Si la multiplicación es menor a cero, entonces la raíz está entre ese intervalo
        ! y por lo tanto, nuestra aproximación será el nuevo límite superior
        if(upper*lower.lt.0.0) then
            upper_lim = mean
        ! Si no se cumple la condición anterior, entonces la raíz está en el otro intervalo
        ! por lo tanto, el límite menor será nuestra aproximación
        else
            lower_lim = mean
        end if

        ! Si la aproximación obtenida no es cero, entonces calculamos el error relativo
        if(mean.ne.0.0) then
            error = abs((x_old - mean)*100/mean)
        end if

        ! Guardamos el valor obtenido para la siguiente iteración
        x_old = mean
        
        
    end do

    ! Finalmente, mostramos en consola el valor de la raíz obtenida
    write(*,*) "Root is: ", mean
    
end program bisection

! Creamos nuestra función cuadrática. x^2 + 6x - 20
function quadratic(x)
implicit none
    real quadratic, x
    quadratic = x**2.0 + 6*x -20
    return
end function
