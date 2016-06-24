module MyAnimation where

import Animation




greenGrass = rgb 0.09 0.91 0.14
skyBlue = rgb 0.16 0.69 1
darkYellow = rgb 0.94 0.83 0.11
lightYellow = rgb 0.96 1 0

propeller = polygon [(0,0), (150, 0), (150, -2),(60, -20),(0,-8)]
tower = polygon [(170 ,215), (195,215),(220,  550),(165,550)]
xPosMinus = -600
xPos = 400
yPos = 100
yPos2 = 550

picture :: Animation
picture =
    background
 `plus`
     sun
 `plus`
     cloud 
  `plus`
     windTurbine

background:: Animation
background =
  --green background representing a green field
  translate(always(0, 236))
   (withPaint(always (greenGrass))
      (rect (always 700) (always 200) ))
 `plus`
 --blue background representing the sky
  (withPaint(always (skyBlue))
      (rect (always 700) (always 250) ))

sun:: Animation
sun =
  --sun
   translate(always (400,50))
     (withPaint (always (darkYellow))
        (circle (cycleSmooth 3([30,33]))))

cloud:: Animation
cloud = 
 -- clouds
    --makes the clouds go all the way around outside of view to come back to original position and start again.
    (combine[translate(cycleSmooth 10[((xPosMinus+p),(yPos+p)), ((xPos+p),(yPos+p)), ((xPos+p),(yPos2+p)), ((-xPosMinus+p),(yPos2+p)), ((-xPosMinus+p),(yPos+p))])(
    (combine[translate(always ((300+x), (-100+y)))  
      (withPaint (always white)
         (circle(always 14))) | x <- [18,36,51,69], y <- [20,35] ])) | p <-[0,90]])  

windTurbine:: Animation
windTurbine =
  -- the wind turbine tower
  (combine[translate(always(n*10,140))(
    scale(always(x*0.2,x*0.2) )(
     (withBorder (always black) (always 1) ) (withPaint (always white) (tower))
 `plus` 
 --the inner circle behind the propellers
 (translate (always (183 , 210))(
       (withBorder (always black) (always 1)  (withPaint (always white) (circle (always 20))))
 `plus`
 --the propellers for the wind turbine
 (combine[(withBorder (always black) (always 1)
    (rotate(spinner(8))
        (rotate (always (c*120))
            ((withPaint (always white)  
               (propeller)))))) |c <- [1..3]])  
  `plus` 
  --the outer cricle on top of the starting points of the blades
     (rotate (spinner 20) (withBorder (always black) (always 1.5)  
        (withPaint (always white) (circle (always 14))))))))) | n <-[1, 25, 50], x<-[1,2.2]])


test:: IO ()
test = writeFile "test.svg" (svg 700 400 picture)

