set terminal gif animate enhanced font "arial,10" size 800,600
set output './results/animation.gif'

set xyplane at -0.2
set zrange [-0.2:0.75]
set cbrange [-0.1:0.13]
set hidden3d nooffset
set pm3d
set xlabel 'x'
set ylabel 'y'
set zlabel 'u(t,x,y)'

plot_file(file) = sprintf("./results/data_step_%04d.dat", file)
system(sprintf("echo "))
system(sprintf("echo start plotting:"))
do for [step=0:1000:5] {
    set title sprintf("Timestep %d", step)
    system(sprintf("bash progress_bar.sh %d %d", step / 5, 1000 / 5))
    splot plot_file(step) w pm3d notitle
    pause 0.1
}
system(sprintf("echo "))