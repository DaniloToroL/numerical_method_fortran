program false_position

    !Programa de búsqueda de raíces con el método de la falsa posición
    ! Autor: Danilo toro
    implicit none 

    ! Definimos las variables que se usarán
    ! upper_lim y lower_lim: Límite superior e inferior
    ! x: Aproximación de la raiz, mediante el promedio
    ! x_old: Última aproximación obtenida
    real upper_lim, lower_lim, x, x_old
    ! error: Error relativo, valor absoluto de (x - x_old) / x
    ! lower y upper: Valor obtenido al reemplazar en la función el límite inferior y superior
    ! tolerance: Error mínimo aceptado para dejar de iterar
    real error, lower, upper, tolerance
    ! quadratic: Nombre de la función (función cuadrática)
    ! denominator: Denominador de la fórmula de aproximación de la raiz
    ! numerator: Numerador de la fórmula de aproximación de la raíz
    real quadratic, denominator, numerator


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
        numerator = quadratic(upper_lim)*(lower_lim - upper_lim)
        denominator = quadratic(lower_lim) - quadratic(upper_lim)
        ! Si el denominador no es 0, calcula la aproximación
        if (denominator.ne.0.0) then
            x = upper_lim - numerator / denominator 
        end if
        ! EVALUAMOS LOS LÍMITES EN LA FUNCIÓN 
        ! Se decidió comenzar desde el límite inferior hasta la mitad del intervalo
        upper = quadratic(x)
        lower = quadratic(lower_lim)

        ! EVALUAMOS LAS CONDICIONES
        ! Si la multiplicación es menor a cero, entonces la raíz está 
        ! entre ese intervalo y por lo tanto, nuestra aproximación será 
        ! el nuevo límite superior
        if(upper*lower.lt.0.0) then
            upper_lim = x
        ! Si la multiplicación es mayor a cero, entonces la raíz está en
        ! el otro intervalo, por lo tanto, el límite menor será nuestra
        ! aproximación
        else if(upper*lower.gt.0.0) then
            lower_lim = x
        ! Si no se cumple ninguna condición, es porque encontramos la raíz
        else
            exit
        end if

        ! Si la aproximación obtenida no es cero, entonces calculamos el error relativo
        if(x.ne.0.0) then
            error = abs((x_old - x)*100/x)
        end if

        ! Guardamos el valor obtenido para la siguiente iteración
        x_old = x
        
        
    end do

    ! Finalmente, mostramos en consola el valor de la raíz obtenida
    write(*,*) "Root is: ", x
    
end program false_position

! Creamos nuestra función cuadrática. x^2 + 6x - 20
function quadratic(x)
implicit none
    real quadratic, x
    quadratic = x**2.0 + 6*x -20
    return
end function
