import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation
import matplotlib.widgets
import matplotlib.font_manager
from matplotlib.widgets import Button
import sys, time, math
import serial
import datetime
#from lib import lib

from pygame import mixer
#sms = lib.utils.sms["@1.0.9"]

#result = sms(to="4168217746",body="The oven is finished")


tempList = []
smallest = 99999
biggest = 0

ser = serial.Serial(
    port='COM5',
    baudrate=57600, #115200
    parity=serial.PARITY_NONE,
    stopbits=serial.STOPBITS_TWO,
    bytesize=serial.EIGHTBITS )
ser.isOpen()


xsize=50
temp = 0
start_time=0
current_time=sys.maxsize
stop_ploting = False
static_scaling = False
   
def data_gen():
    global smallest, biggest,current_time,start_time
    t = data_gen.t
    tempList.append(23)
    while True:
        strin = ser.readline()
        try:
            val = int(chr(strin[0]) + chr(strin[1]) + chr(strin[2]),10)
        except:
            val = tempList[-1] 

        t+=1
        tempList.append(val)

        for i in tempList:
            if(i < smallest):
                smallest = i
            if(i>biggest):
                biggest = i
        if val > 250 and not stop_ploting:
            if current_time-start_time > 1:
                start_time = time.time()
                print("\a")
            current_time = time.time()
            ax.set_facecolor('xkcd:salmon')  
        else:
            ax.set_facecolor('xkcd:white')

        yield t, val

def run(data):

    global stop_ploting,static_scaling,smallest,biggest
    # update the data
    t,y = data
    currentDT = datetime.datetime.now()
    ax.set_title("Hello Oven Operator, current time is "+str(currentDT)[11:19], loc="center")
    if t>-1:
        xdata.append(t)
        ydata.append(y)

        if not stop_ploting:
            line.set_data(xdata, ydata)
            if static_scaling:
                ax.set_ylim(0,300)
                ax.set_xlim(0,300)
            else:
                ax.set_ylim(smallest-1, biggest+1)
                ax.set_xlim(0, t+ 1)

    return line,


def on_close_figure(event):
    sys.exit(0)


class Index(object):
    def stop(self,event):
        global stop_ploting
        if stop_ploting:
            stop_ploting = False
        else:
            stop_ploting = True

    def scale(self,event):
        global static_scaling
        if static_scaling:
            static_scaling = False
        else:
            static_scaling = True
        


data_gen.t = -1
fig = plt.figure()
fig.canvas.mpl_connect('close_event', on_close_figure)
fig.canvas.set_window_title('Tempertature reading from sensor')
ax = fig.add_subplot(111)
line, = ax.plot([], color='red',linestyle='-', lw=2)
ax.set_ylim(15, 35)
ax.set_xlim(0, xsize)
ax.grid()
xdata, ydata = [], []
plt.xlabel("Time (Seconds)")
plt.ylabel("Temperature (Celcius)")



callback = Index()

axstop = plt.axes([0.9, 0.115, 0.1, 0.075])
axscale = plt.axes([0.9, 0.2, 0.1, 0.075])

bscale = Button(axscale, 'Scale')
bstop = Button(axstop, 'Stop')

bstop.on_clicked(callback.stop)
bscale.on_clicked(callback.scale)


mixer.init()
mixer.music.load('song6.ogg')
mixer.music.play()

#ax.text(xsize, 30, "Hello", bbox=dict(facecolor='red', alpha=0.5))
# Important: Although blit=True makes graphing faster, we need blit=False to prevent
# spurious lines to appear when resizing the stripchart.
ani = animation.FuncAnimation(fig, run, data_gen, blit=False, interval=100, repeat=False)
plt.show()





