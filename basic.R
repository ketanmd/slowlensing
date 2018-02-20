library(dplyr)
library(magrittr)
library(ggplot2)

spyr = 3e7 # seconds per year
masprad = 206265e3 # milliarcseconds per radian
arcsprad = 206265 # arcseconds per radian
pcpyr   = 0.307 # light speed [parsecs per year]
m_S = 2e30 # mass of sun [kg]
G = 6.67e-11 # grav constant
cee = 3e8 # speed of light [m/s]
mppc = 3.1e16 # meters per parsec

vt <- c(1e-4 * cee,  # orbital velocity of earth around sun
        8e-4 * cee,  # orbital velocity of sun around galaxy
        18e-4 * cee) # galaxy escape velocity at solar position

"

Start with coordinates aligned with observed proper motion of image
_i : image
_s : source
_l : lens
_S : Sun
_r : reference point

d_ : distance
t_ : angular separation
p_ : angular proper motion
v_ : radial velocity
r_ : schwartzchild radius

r  : impact parameter of light passing lens
th : gravitational bending angle

r_l      = 2 * G * m_l / cee / cee
r_S      = 2 * G * m_S / cee / cee

r    = d_l * t_il
th   = 2 * r_l / r
t_si = th * d_sl / d_s

t_si * t_il = 2 * r_l * d_sl / d_s / d_l

ed = d_l/d_s
K  = d_l/(1 - ed)
ed: [0,epsilon]

em = m_l / m_S
r_l = em * r_S
em : [0,epsilon]

ft = t_si / t_il
t_si = ft * t_il
ft : [0,epsilon]

t_il * t_il * ft = em * (1 - ed) * 2 * r_S / d_l

t_il * t_il = ( (1-ed) * em / ft ) * 2 * r_S / d_l


Now, sample reasonable values for everything

what is measured?
t_ri d_i p_ri v_i

Assume the values for:

"
tee     <- 30  # lens time of arrival [years]

em <- c(0.01,0.1,1) # mass of lens / solar mass


v_I <- c( vt, 0, -vt) # image radial velocities [m/s]
d_S.pc <- c(1,10,100,1000)  # distance to source [pc]

frac_L <- c(1e-4,1e-3,1e-2,1e-1) # lens distance to source distance fraction

alphafrac <- c(0.01,0.02,0.05,0.1,0.2,0.5,1.0) # source offset from image

p_S.arcspyr <- p_L.arcspyr <- p_I.arcspyr <-
    c(-1e3,-1e2,-1e1,0,1e1,1e2,1e3) # proper motions [arcspyr]

adf <- expand.grid( m_L.sun,   frac_L,   v_I,   d_S.pc,   p_I.arcspyr,   p_S.arcspyr,   p_L.arcspyr,   alphafrac )
names(adf) <-    c('m_L.sun', 'frac_L', 'v_I', 'd_S.pc', 'p_I.arcspyr', 'p_S.arcspyr', 'p_L.arcspyr', 'alphafrac')

adf <- within(adf, {
    v_S = v_I
    
    d_L.pc  = frac_L * d_S.pc
    k_L    = 1/ d_L.pc / mppc
    k_S    = 1/ d_S.pc / mppc
    r_S    = G * m_L.sun  * m_S / cee / cee
    f      = 2 * r_S * (k_L - k_S)
    alpha  = alphafrac * sqrt(f)
    theta  = f / alpha
    
    f.arcssq   = f * arcsprad * arcsprad
    alpha.arcs = alpha * arcsprad
    theta.arcs = theta * arcsprad

    k_S. = -v_S * k_S * k_S
    k_L. = (((p_I.arcspyr - p_S.arcspyr)/alpha.arcs) +
            ((p_I.arcspyr - p_L.arcspyr)/theta.arcs)  ) * spyr * (k_L - k_S) + k_S.
    v_L = -k_L. * d_L * d_L
    tee  = d_L / v_L / spyr
})

## check inputs carefully.

ggplot(subset(adf, theta.arcs > alpha.arcs)) +
    geom_point(aes(x = alpha.arcs, y = theta.arcs, color = r_S.m)) +
    facet_grid(d_L.pc ~ d_S.pc)