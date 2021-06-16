import Data.Complex
scale::Float->Float->Float->Float->Float->Float
scale i iMin iMax outMin outMax=((i-iMin)/(iMax-iMin))*(outMax - outMin) + outMin


verify::Complex Float->Complex Float->Int->Int->Int 
verify z c i max=
     if realPart  z < 2&&i<max then 
         verify (z**2+c) c (i+1) max 
    else i
mandelbrot::Float ->Float->IO()
mandelbrot width height=do
    mapM_ (\px->do
        let cx =scale px 0 width (-1.511) 1
        mapM_ (\py->do
            let (cy,c,z,max) =(scale py 0 height  (-1)  1,cx:+cy,0:+0,30)
                i=verify z c 0 30
            if i <30 then putStr " " else putStr "#"
            )[1..height]
        putStr "\n"
    
        ) [1..width]
main::IO()
main=mandelbrot 70 70