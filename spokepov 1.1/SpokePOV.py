#!/usr/bin/env python

# The software for SpokePOV is available for use in accordance with the 
# following open source license (MIT License). For more information about
# OS licensing, please visit -> http://www.opensource.org/
#
# For more information about SpokePOV, please visit
# -> http://www.ladyada.net/make/spokepov
#
#                                     *****
# Copyright (c) 2005 Limor Fried
#
# Permission is hereby granted, free of charge, to any person obtaining a 
# copy of this software and associated documentation files (the "Software"), 
# to deal in the Software without restriction, including without limitation 
# the rights to use, copy, modify, merge, publish, distribute, sublicense, 
# and/or sell copies of the Software, and to permit persons to whom the 
# Software is furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in 
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
# FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
# DEALINGS IN THE SOFTWARE.
#                                     *****

import sys, time
import parallel
import array
import wx
import math
import ConfigParser
import pickle

# Note: this is my first Python/wx program. It probably has mistakes, bugs,
# bad code. Oh well.  -- ladyada

# which mode we are when dragging the mouse in the wheel panel
DRAWING = 1
ERASING = 0

PI = 3.1415

#default rotation
autorotation = 0

#default hub diameter is 2" (*4 LEDs per inch)
hubsize = 2*4

#default LEDs per row is 30, BMX wheels have 22
num_leds = 30

############## spoke interface commands, must be the same in firmware!
COMP_CMD_SETFLED = 0x81
COMP_CMD_SETBLED = 0x82
COMP_CMD_CLRFLED = 0x83
COMP_CMD_CLRBLED = 0x84
COMP_CMD_RDEEPROM = 0x85
COMP_CMD_WREEPROM = 0x86
COMP_CMD_RDEEPROM16 = 0x87
COMP_CMD_WREEPROM16 = 0x88
COMP_SUCCESS = 0x80

############## spokepov parallel port pins
SPI_CLK = 3;    # DATA3
SPI_DO = 0;     # DATA0

############## graphics constant: size of the wheel panel. 600x600 seems good
WHEEL_H = 600
WHEEL_W = 600

# how many radial lines to be swept out per rev, should match firmware
ROWS_PER_WHEEL = 256         

# variables stored in the microcontroller
EEPROM_ROTOFFSET = 0x8000
EEPROM_MIRROR = 0x8001

####################################################3
# this class talks to the actual spokes
########
class SpokePOVComm:
    def __init__(self):
        self.p = parallel.Parallel()  # open LPT1
        self.out(0xFF)

    # if there's a spokepov on the other side, this pin should be low
    def connected(self):
        r = ~ self.p.getInAcknowledge()
        return r

    # since theres only a byte-wide byte output function, these functions
    # allow setting and clearing individual pins
    def out(self, b):
        self.data = b
        self.p.setData(self.data)

    def setbit(self, b):
        self.data |= 1 << b
        self.p.setData(self.data)

    def clrbit(self, b):
        self.data &= ~(1 << b)
        self.p.setData(self.data)

    # bit-bang the SPI protocol over to the microcontroller
    def spi_xfer(self, c):
        #print "sending %02x\n" % c
        x = 7
        ret = 0
        
        while x >= 0:
            
            if (self.connected() == 0):
                raise IOError, "not connected"

            # send data out
            if c & (1 << x):
                self.setbit(SPI_DO)
            else:
                self.clrbit(SPI_DO)
            self.setbit(SPI_CLK)

            #print "^"
            #ya = raw_input()

            time.sleep(0.001)

            # get data in
            ret <<= 1
            ret |= self.p.getInBusy()
            
            self.clrbit(SPI_CLK)
            time.sleep(0.001)

            #print "v"
            #ya = raw_input()

            
            x -= 1

        #print "got %02x" % ret
        return ret

    # read one byte from the external EEPROM
    def read_eeprom(self, addr):
        self.spi_xfer(COMP_CMD_RDEEPROM)
        #time.sleep(0.01)
        self.spi_xfer((addr >> 8) & 0xFF)
        #time.sleep(0.01)
        self.spi_xfer(addr & 0xFF)
        #time.sleep(0.01)
        val = self.spi_xfer(0)
        #time.sleep(0.01)
        ret = self.spi_xfer(0);
        if (ret != 0x80):
            print "failed! 0x%02x" % ret
            raise IOError, "Didn't succeed"        
        return val

    # read 16 bytes from the external EEPROM (faster!)
    def read_eeprom16(self, addr):
        self.spi_xfer(COMP_CMD_RDEEPROM16)
        self.spi_xfer((addr >> 8) & 0xFF)
        self.spi_xfer(addr & 0xFF)
        x = 16
        val = []
        while (x != 0):
            val.append(self.spi_xfer(0))
            x -= 1
            
        ret = self.spi_xfer(0);
        if (ret != 0x80):
            raise IOError, "Didn't succeed"        
        return val

    
    def write_eeprom(self, addr, val):
        self.spi_xfer(COMP_CMD_WREEPROM)
        self.spi_xfer((addr >> 8) & 0xFF)
        self.spi_xfer(addr & 0xFF)
        self.spi_xfer(val)
        ret = self.spi_xfer(0);
        if (ret != 0x80):
            raise IOError, "Didn't succeed"        

    
    def write_eeprom16(self, addr, val):
        self.spi_xfer(COMP_CMD_WREEPROM16)
        self.spi_xfer((addr >> 8) & 0xFF)
        self.spi_xfer(addr & 0xFF)
        x = 0
        while (x != 16):
            self.spi_xfer(val[x])  
            x += 1
            
        ret = self.spi_xfer(0);
        if (ret != 0x80):
            raise IOError, "Didn't succeed"        
    
    def sendcmd(self, cmd, arg):
        self.spi_xfer(cmd)
        time.sleep(0.005)
        self.spi_xfer(arg)
        time.sleep(0.005)
        ret = self.spi_xfer(0);
        if (ret != 0x80):
            raise IOError, "Didn't succeed"




# This model is like an array, except I couldnt figure out how to get
# arrays to do what I want. This also keeps track of whether its been
# modified, which speeds up drawing
class WheelModel:
    def __init__(self):
        self.arr = [None]*ROWS_PER_WHEEL
        self.row = [None]*num_leds
        
    def __getitem__(self, (i, j)):
        return (self.arr[i] or self.row)[j]

    def __setitem__(self, (i, j), value):
        if self.arr[i]==None: self.arr[i] = self.row[:]
        self.arr[i][j] = value
        self.changed = 1

    def has_changed(self):
        return self.changed
    def set_changed(self, f):
        self.changed = f
    
    def clone(self):
        c = WheelModel()
        
        for i in range(0,num_leds):
            for j in range(0,ROWS_PER_WHEEL):
                c[(i, j)] = self[(i,j)]
        return c

# The model for the data
model = WheelModel()


########################################
# the panel widget that deals with clicks and drawing & stuff
#############

class WheelPanel(wx.Panel):
    image = None

    def __init__(self, parent):
        global model
        wx.Panel.__init__(self, parent)

        # initialize the model
        for x in range(0 , ROWS_PER_WHEEL):
            for y in range(0 , num_leds):
                model[(x, y)] = 0

        (width, height) = self.GetSizeTuple()
        self._BackgroundBuffer = None
        self._ForegroundMaskBuffer = None

        # the red buffer is a large red square which is then
        # masked by the pixel mask, which makes it much faster to
        # draw and erase the images. 
        self.RedBuffer = wx.EmptyBitmap(width, height)
        reddc = wx.MemoryDC()
        reddc.SelectObject(self.RedBuffer)
        reddc.SetPen(wx.RED_PEN)
        reddc.SetBrush(wx.RED_BRUSH)
        reddc.DrawRectangle(0, 0, width, height)
        reddc.SelectObject(wx.NullBitmap)
        self.mask = None
        
        self.SetBackgroundColour(wx.NamedColour('white'))
        self.Bind(wx.EVT_PAINT, self.OnPaint)
        self.SetFocus()
        self.Bind(wx.EVT_LEFT_DOWN, self.OnLeftDown)
        self.Bind(wx.EVT_LEFT_UP, self.OnLeftUp)
        self.Bind(wx.EVT_MOTION, self.OnMotion)
        self.drawstate = None

    def OnSize(self, event):
        self._BackgroundBuffer = wx.EmptyBitmap(self.Width, self.Height)
        self._ForegroundMaskBuffer = wx.EmptyBitmap(self.Width, self.Height)
        self.UpdateBackground()
        self.Refresh()

    # called when the model changed and the foreground mask must be recalculated
    def UpdateForegroundMask(self):
        global model, hubsize

        (width, height) = self.GetSizeTuple()

        self._ForegroundMaskBuffer = wx.EmptyBitmap(width, height) 

        dc = wx.MemoryDC()

        dc.SelectObject(self._ForegroundMaskBuffer)

        dc.Clear();
        
        middle = wx.Point(width/2, height/2)
        dDiameter = min(width, height) / (num_leds + hubsize)

        # draw the pixels
        pen = wx.Pen("green", dDiameter/2, wx.SOLID)
        pen.SetCap(wx.CAP_BUTT)
        dc.SetPen(pen)
        dc.SetBrush(wx.TRANSPARENT_BRUSH)
        
        dAngle = 360.0/ROWS_PER_WHEEL

        for j in range(0,num_leds):            
            endangle = startangle = -1
            diam = (j+.5+hubsize) * dDiameter
            for i in range(0, ROWS_PER_WHEEL):
                if (model[(i,j)] == 0):
                    if (endangle == -1):
                        continue
                    # draw this sweep
                    #print (startangle, endangle)
                    dc.DrawEllipticArc((middle.x - (diam/2) + .5),
                                       (middle.y - (diam/2) + .5), 
                                       diam, diam,
                                       startangle, endangle)

                    startangle = endangle = -1
                    
                if (model[(i,j)] == 1):
                    if (endangle == -1):
                        endangle = 90.0 - (i*360.0/ROWS_PER_WHEEL)
                        startangle = endangle
                    startangle -= dAngle
                    
                    if (startangle != -1):
                        dc.DrawEllipticArc((middle.x - (diam/2) + .5),
                                           (middle.y - (diam/2) + .5), 
                                           diam, diam,
                                           startangle, endangle)
        dc.SelectObject(wx.NullBitmap)

        
    # call this when you want to draw the window from scratch
    # (and update the double buffer)
    # ie when a new image is imported
    def UpdateBackground(self):
        global hubsize

        print "bg update"
            
        (width, height) = self.GetSizeTuple()


        self.RedBuffer = wx.EmptyBitmap(width, height)
        reddc = wx.MemoryDC()
        reddc.SelectObject(self.RedBuffer)
        reddc.SetPen(wx.RED_PEN)
        reddc.SetBrush(wx.RED_BRUSH)
        reddc.DrawRectangle(0, 0, width, height)
        reddc.SelectObject(wx.NullBitmap)

        self._BackgroundBuffer = wx.EmptyBitmap(width, height) 

        dc = wx.MemoryDC()

        dc.SelectObject(self._BackgroundBuffer) 
             
        # clear up the background
        dc.Clear();
        #self.SetBackgroundColour((255,255,255))

        middle = wx.Point(width/2, height/2)
        dDiameter = min(width, height) / (num_leds + hubsize)

        # ok draw the image (if there is one)
        if (self.image != None):
            dc.DrawBitmap(self.image.ConvertToBitmap(), 0, 0)

        # set up the pen for drawing: black outlines
        dc.SetPen(wx.GREY_PEN)
        dc.SetBrush(wx.TRANSPARENT_BRUSH)

        # ok draw the rings
        for ring in range(0, num_leds+1):
            diam = dDiameter * (hubsize+ring)
            dc.DrawCircle(middle.x, middle.y, diam/2)

        # OK draw the radials
        innerringD = dDiameter*hubsize
        outerringD = dDiameter*(num_leds+hubsize)
        for rad in range(0, ROWS_PER_WHEEL):
            x1 = innerringD/2 * math.sin(2*PI * rad / ROWS_PER_WHEEL)
            x2 = outerringD/2 * math.sin(2*PI * rad / ROWS_PER_WHEEL)
            y1 = innerringD/2 * math.cos(2*PI * rad / ROWS_PER_WHEEL)
            y2 = outerringD/2 * math.cos(2*PI * rad / ROWS_PER_WHEEL)
            dc.DrawLine(middle.x+x1, middle.y+y1, middle.x+x2, middle.y+y2)

        dc.SelectObject(wx.NullBitmap)
        # now call the buffered drawing procedure
        self.Draw()
        
        
    def Draw(self, dc = None):
        global model, hubsize

        (width, height) = self.GetSizeTuple()
        
        if not dc:
            dc = wx.ClientDC(self)

        if (self._BackgroundBuffer == None):
            self.UpdateBackground()
        
        if ((self._ForegroundMaskBuffer == None) or model.has_changed()):
            self.UpdateForegroundMask()
            model.set_changed(False)


        BlitBuffer = wx.EmptyBitmap(width, height)
        blitdc = wx.MemoryDC()
        blitdc.SelectObject(BlitBuffer)

        blitdc.DrawBitmap(self._BackgroundBuffer,0,0)
        self.mask = wx.Mask(self._ForegroundMaskBuffer, wx.WHITE)
        # mask with white transparent
        self.RedBuffer.SetMask(self.mask)
        blitdc.DrawBitmap(self.RedBuffer,0,0, True)
        blitdc.SelectObject(wx.NullBitmap)

        dc.DrawBitmap(BlitBuffer, 0, 0)
               
    def OnPaint(self, event):
        dc = wx.PaintDC(self)
        self.Draw(dc)

    def setImage(self, image):
        global model
        
        if (image == None):
            return
            
        (width, height) = self.GetSizeTuple()
        self.image = image.Scale(width, height)
        self.image.ConvertToMono(0,0,0)
        imagedata = self.image.GetData()
        #print len(imagedata)
        
        for j in range(0,height):
            s = ""
            for i in range(0,width):
                s += str(imagedata[ (j*width+i)*3 ])+" "
            #print s
                
        # create model from image
        for j in range(0,ROWS_PER_WHEEL):
            for i in range(0,num_leds):
                (x, y) = self.getXYPointForLED(i, j)
                foo = (int(x)*width+int(y))*3
                pix = ord(imagedata[foo])
                if (pix == 0):
                    model[(j,i)] = 1
                else:
                    model[(j,i)] = 0
        
        self.UpdateBackground()
        self.Draw()

    def getLEDForXYPoint(self, x, y):
        (width, height) = self.GetSizeTuple()
        dDiameter = min(width, height) / (num_leds + hubsize)

        # find led num by finding distance from point to middle
        lednum = int(math.sqrt( math.pow((width/2 - x), 2) + math.pow((height/2 - y), 2) ) / (dDiameter / 2) - hubsize)

        #find row by calculating angle
        x -= width/2
        y -= height/2

        radius = math.sqrt( math.pow(x, 2) + math.pow(y, 2) )
        angle = math.asin(x/radius) * 180/PI

        angle = (angle + 360) % 360
        if ((x > 0) and (y > 0)):
        angle = 180 - angle
        if  ((x < 0) and (y > 0)):
            angle = 360 - angle + 180

        return (lednum, int(angle / 360 * ROWS_PER_WHEEL))

    def getXYPointForLED(self, led, row):
        (width, height) = self.GetSizeTuple()
        dDiameter = min(width, height) / (num_leds + hubsize)

        radius = (led + .5 + hubsize)*dDiameter/2
        startangle = 270 - ((row+.5) * 360.0)/ROWS_PER_WHEEL
        angle = startangle * PI/180.0
        
        return ( (math.sin(angle)*radius+(width/2),
                  (height/2)-math.cos(angle)*radius))

    def OnLeftDown(self, event):
        global model, num_leds, DRAWING, ERASING
        
        self.CaptureMouse()
        # figure out if we're erasing or drawing 
        (lednum, rownum) = self.getLEDForXYPoint(event.GetPosition().x, event.GetPosition().y)
        #print (lednum, rownum)
        if ((lednum < 0) or (lednum > num_leds)):
            return

        # trick the double buffer, set the value, draw on the mask, then clear the modelchange
        (width, height) = self.GetSizeTuple()
        dc = wx.MemoryDC()
        dc.SelectObject(self._ForegroundMaskBuffer)

        startangle = 90.0 - ((rownum+1)*360.0/ROWS_PER_WHEEL)
        dAngle = 360.0/ROWS_PER_WHEEL
        dDiameter = min(width, height) / (num_leds + hubsize)
        diam = (lednum+.5+hubsize) * dDiameter
        dc.SetBrush(wx.TRANSPARENT_BRUSH)
        if (model[(rownum, lednum)] == 0):
            pen = wx.Pen("green", dDiameter/2, wx.SOLID)
            model[(rownum, lednum)] = 1
            self.drawstate = DRAWING
        else:
            pen = wx.Pen("white", dDiameter/2, wx.SOLID)
            model[(rownum, lednum)] = 0
            self.drawstate = ERASING
        pen.SetCap(wx.CAP_BUTT)
        dc.SetPen(pen)
        dc.DrawEllipticArc((width/2 - (diam/2) + .5),
                           (height/2 - (diam/2) + .5), 
                           diam, diam,
                           startangle,startangle+dAngle)
        dc.SelectObject(wx.NullBitmap)
        model.set_changed(False) 

        self.Draw()
        
    def OnLeftUp(self, event):
        if self.HasCapture():
            self.ReleaseMouse()
        self.drawstate = None
        
    def OnMotion(self, event):
        global DRAWING, ERASING
        
        if (self.drawstate != None):
            (lednum, rownum) = self.getLEDForXYPoint(event.GetPosition().x, event.GetPosition().y)
            if ((lednum < 0) or (lednum >= num_leds)):
                return

            # trick the double buffer, set the value, draw on the mask, then clear the modelchange
            (width, height) = self.GetSizeTuple()
            dc = wx.MemoryDC()
            dc.SelectObject(self._ForegroundMaskBuffer)

            startangle = 90.0 - ((rownum+1)*360.0/ROWS_PER_WHEEL)
            dAngle = 360.0/ROWS_PER_WHEEL
            dDiameter = min(width, height) / (num_leds + hubsize)
            diam = (lednum+.5+hubsize) * dDiameter
            dc.SetBrush(wx.TRANSPARENT_BRUSH)
            if (self.drawstate == DRAWING):
                pen = wx.Pen("green", dDiameter/2, wx.SOLID)
                model[(rownum, lednum)] = 1
            else:
                pen = wx.Pen("white", dDiameter/2, wx.SOLID)
                model[(rownum, lednum)] = 0

            pen.SetCap(wx.CAP_BUTT)
            dc.SetPen(pen)
            dc.DrawEllipticArc((width/2 - (diam/2) + .5),
                               (height/2 - (diam/2) + .5), 
                               diam, diam,
                               startangle,startangle+dAngle)
            dc.SelectObject(wx.NullBitmap)
            model.set_changed(False)
            self.Draw()

class SetRotationDialog(wx.Dialog):
    def __init__(
            self, parent, ID, title, size=wx.DefaultSize, pos=wx.DefaultPosition, 
            style=wx.DEFAULT_DIALOG_STYLE, offset=0
            ):

        self.parent = parent
        # Instead of calling wx.Dialog.__init__ we precreate the dialog
        # so we can set an extra style that must be set before
        # creation, and then we create the GUI dialog using the Create
        # method.
        pre = wx.PreDialog()
        pre.SetExtraStyle(wx.DIALOG_EX_CONTEXTHELP)
        pre.Create(parent, ID, title, pos, size, style)

        # This next step is the most important, it turns this Python
        # object into the real wrapper of the dialog (instead of pre)
        # as far as the wxPython extension is concerned.
        self.PostCreate(pre)

        # Now continue with the normal construction of the dialog
        # contents
        sizer = wx.BoxSizer(wx.VERTICAL)

        label = wx.StaticText(self, -1, "Set the rotation offset")
        sizer.Add(label, 0, wx.ALIGN_CENTRE|wx.ALL, 5)

        box = wx.BoxSizer(wx.HORIZONTAL)

        label = wx.StaticText(self, -1, "Offset:")
        box.Add(label, 0, wx.ALIGN_CENTRE|wx.ALL, 5)

        self.text = wx.TextCtrl(self, -1, str(offset), size=(80,-1))
        box.Add(self.text, 1, wx.ALIGN_CENTRE|wx.ALL, 5)
        sizer.Add(box, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)

        line = wx.StaticLine(self, -1, size=(20,-1), style=wx.LI_HORIZONTAL)
        sizer.Add(line, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.RIGHT|wx.TOP, 5)

        btnsizer = wx.BoxSizer(wx.HORIZONTAL)
         
        btn = wx.Button(self, -1, "Write")
        btn.SetDefault()
        self.Bind(wx.EVT_BUTTON, self.OnWriteButton, btn)
        btnsizer.Add(btn)

        btn = wx.Button(self, wx.ID_CANCEL, "Done")
        btnsizer.Add(btn)

        sizer.Add(btnsizer, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)

        self.SetSizer(sizer)
        sizer.Fit(self)

    def OnWriteButton(self, evt):
        spov = SpokePOVComm()
        try:
            spov.write_eeprom(EEPROM_ROTOFFSET, int(self.text.GetLineText(0)))
            self.parent.SetStatusText("Wrote rotation offset successfully")
        except IOError:
            self.parent.SetStatusText("IO ERROR: Could not write rotation offset!")


#---------------------------------------------------------------------------

###################################
# The top-level frame (menus, buttons, wheelpanel)
####

class SpokeSoftFrame(wx.Frame):
    """
    This is the spokesoft Frame
    """

    wheel = None
    filename = None
    
    def __init__(self, parent, title):
        global autorotation
        
        wx.Frame.__init__(self, parent, -1, title,
                          pos=(50, 50), size=(WHEEL_W+6, WHEEL_H+130),
                          style=(wx.DEFAULT_FRAME_STYLE & ~(wx.RESIZE_BORDER | wx.RESIZE_BOX | wx.MAXIMIZE_BOX)))

        self.SetTitle("pySpokePOV            ")
        # Create the menubar
        menuBar = wx.MenuBar()

        # and a menu 
        filemenu = wx.Menu()

        filemenu.Append(0, "Open...", "Open a SpokePOV image file")
        filemenu.Append(1, "Import...",  "Import a BMP file")
        filemenu.Append(2, "Save",  "Save SpokePOV image to a file")
        filemenu.Append(3, "Save As...",  "Save the SpokePOV image to a new file")
        filemenu.AppendSeparator()
        filemenu.Append(4, "Set Rotation...", "Set the rotation value for the SpokePOV")
        filemenu.AppendSeparator()
        filemenu.Append(wx.ID_EXIT, "E&xit", "Exit")

        # bind the menu events to event handlers
        # import image
        self.Bind(wx.EVT_MENU, self.OnImportBMP, id=1)
        # Exit
        self.Bind(wx.EVT_MENU, self.OnTimeToClose, id=wx.ID_EXIT)
        # set rotation
        self.Bind(wx.EVT_MENU, self.OnSetRotation, id=4)
        # save
        self.Bind(wx.EVT_MENU, self.OnSave, id=2)
        self.Bind(wx.EVT_MENU, self.OnSaveAs, id=3)
        self.Bind(wx.EVT_MENU, self.OnOpen, id=0)
        
        # and put the menu on the menubar
        menuBar.Append(filemenu, "&File")

        
        self.SetMenuBar(menuBar)

        self.CreateStatusBar()
        
        # Now create the Panel to put the other controls on.
        mainpanel = wx.Panel(self)
        mainpanelsizer = wx.BoxSizer(wx.VERTICAL)
        
        # a wheelpanel
        self.wheel = WheelPanel(mainpanel)
        self.wheel.SetSize((WHEEL_W,WHEEL_H))

        mainpanelsizer.Add(self.wheel, 0, wx.ALL  | wx.ALIGN_CENTER, 0)
        lowerpanel = wx.Panel(mainpanel)
        lowerpanelsizer = wx.BoxSizer(wx.HORIZONTAL)
        
        # create 3 buttons: read, write, verify
        buttonpanel = wx.Panel(lowerpanel)
        buttonpanelsizer = wx.GridSizer(1, 3, 5, 10)
        readbutton = wx.Button(buttonpanel, -1, "Read")
        writebutton = wx.Button(buttonpanel, -1, "Write")
        verifybutton = wx.Button(buttonpanel, -1, "Verify")

        self.Bind(wx.EVT_BUTTON, self.OnReadButton, readbutton);
        self.Bind(wx.EVT_BUTTON, self.OnWriteButton, writebutton);
        self.Bind(wx.EVT_BUTTON, self.OnVerifyButton, verifybutton);

        buttonpanelsizer.Add(readbutton, 0, wx.ALL, 0)
        buttonpanelsizer.Add(writebutton, 0, wx.ALL, 0)
        buttonpanelsizer.Add(verifybutton, 0, wx.ALL, 0)
        buttonpanel.SetSizer(buttonpanelsizer)
        buttonpanel.Layout()

        lowerpanelsizer.Add(buttonpanel, 1, wx.EXPAND|wx.ALL, 10)
        
        radiopanel = wx.Panel(lowerpanel)
        radiopanelsizer = wx.FlexGridSizer(1, 2, 5, 5)
        radiobuttonpanel = wx.Panel(radiopanel)
        radiobuttonpanelsizer = wx.FlexGridSizer(2, 1, 5, 5)
        self.mirror_yes = wx.RadioButton(radiobuttonpanel, -1, " Yes ", style = wx.RB_GROUP )
        self.mirror_no = wx.RadioButton(radiobuttonpanel, -1, " No " )

        radiobuttonpanelsizer.Add(self.mirror_yes, 0, wx.ALL, 0)
        radiobuttonpanelsizer.Add(self.mirror_no, 0, wx.ALL, 0)
        radiobuttonpanel.SetSizer(radiobuttonpanelsizer)
        radiobuttonpanel.Layout()

        label = wx.StaticText(radiopanel, -1, "Mirror image on other side?")
        radiopanelsizer.Add(label, 0, wx.ALIGN_CENTRE|wx.ALL, 5)
        radiopanelsizer.Add(radiobuttonpanel, 0, wx.ALIGN_CENTRE|wx.ALL, 5)
        radiopanel.SetSizer(radiopanelsizer)
        radiopanel.Layout()

        lowerpanelsizer.Add(radiopanel, 1, wx.EXPAND|wx.ALL, 0)
        lowerpanel.SetSizer(lowerpanelsizer)
        lowerpanelsizer.Layout()

        mainpanelsizer.Add(lowerpanel, 0, wx.ALL | wx.ALIGN_CENTER, 10)
      
        mainpanel.SetSizer(mainpanelsizer)
        
        mainpanel.Layout()
        self.wheel.UpdateBackground()  # create the doublebuffered background
        self.wheel.Draw()

    def OnSetRotation(self, evt):
        spov = SpokePOVComm()
        try:
            offset = spov.read_eeprom(0x8000)
            self.SetStatusText("Read rotation offset successfully")
        except IOError:
            self.SetStatusText("IO ERROR: Could not read rotation offset!")
            return
            
        dlg = SetRotationDialog(self, -1, "Set Rotation Offset", size=(350, 200),
                         #style = wxCAPTION | wxSYSTEM_MENU | wxTHICK_FRAME
                         style = wx.DEFAULT_DIALOG_STYLE, offset=offset
                         )
        dlg.CenterOnScreen()

        # this does not return until the dialog is closed.
        val = dlg.ShowModal()
    
        dlg.Destroy()
        

    def OnVerifyButton(self, evt):
        global model
        failed = False
        
        spov = SpokePOVComm()
        try:
            i = 0
            lasttime = time.time()

            while (i < (ROWS_PER_WHEEL * 4)):
                foo16 = spov.read_eeprom16(i)
                self.SetStatusText('Reading address '+str(i))
                for j in range(0, 16):
                    foo = ~(foo16[j])
                    for y in range(int((i+j)%4)*8, min(30, int((i+j)%4)*8+8) ):
                        if (model[(int((i+j)/4), y)] != (foo >> 7) & 0x1):
                            self.SetStatusText("Verification Failed!")
                            return
                        foo = foo << 1
                i += 16

            if (not ((self.mirror_yes.GetValue() == 0) and
                     (spov.read_eeprom(EEPROM_MIRROR) == 0))):
                self.SetStatusText("Verification Failed!")
                return
            
            self.SetStatusText("Verification sucessful!")

        except IOError:
            self.SetStatusText("IO ERROR: died at address %d" % i)
            
    def OnReadButton(self, evt):
        global model
        
        read = WheelModel()
        
        spov = SpokePOVComm()
        try:
            i = 0
            lasttime = time.time();

            while i < (ROWS_PER_WHEEL * 4):
                foo16 = spov.read_eeprom16(i)
                self.SetStatusText('Reading address '+str(i))
                for j in range(0, 16):
                    foo = ~(foo16[j])
                    for y in range(int((i+j)%4)*8, min(30, int((i+j)%4)*8+8) ):
                        read[(int((i+j)/4), y)] = (foo >> 7) & 0x1
                        foo = foo << 1
                i += 16
                
            # read the mirror value
            self.mirror_yes.SetValue(spov.read_eeprom(EEPROM_MIRROR))
            self.mirror_no.SetValue(not spov.read_eeprom(EEPROM_MIRROR))
                
            self.SetStatusText("Took %2.1f seconds" % (time.time() - lasttime))
            model = read
            
            self.wheel.Draw()
        except IOError:
            self.SetStatusText("IO ERROR: died at address %d" % i)
        
    def OnWriteButton(self, evt):
        global model
        
        spov = SpokePOVComm()
        try:
            i = 0
            lasttime = time.time();
            while i < (ROWS_PER_WHEEL * 4):
                buff16 = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
                for col in range(0, 4):
                    for row in range(0, 4):
                        foo = 0
                        for y in range(row*8, min(30, (row+1)*8) ):
                            if (y < (30-num_leds)):
                                continue
                            foo = foo << 1
                            foo |= model[((i/4)+col, y-(30-num_leds))] & 0x1
                        # ignore the first two bits (they are not displayed)
                        if (row == 3):
                            foo <<= 2
                        buff16[col*4+row] = ~foo

                self.SetStatusText('Writing address '+str(i))
                spov.write_eeprom16(i, buff16)
                i += 16

            # write the mirror value
            if (self.mirror_yes.GetValue()):
                spov.write_eeprom(EEPROM_MIRROR, 1)
            else:
                spov.write_eeprom(EEPROM_MIRROR, 0)
            
            self.SetStatusText("Took %2.1f seconds" % (time.time() - lasttime))
            
        except IOError:
            self.SetStatusText("IO ERROR: died at address %d" % i)


        
    def OnImportBMP(self, evt):
        #print("importing...")
        dialog = wx.FileDialog(self,
                               "Choose an image to import...",  # title
                               "", "",          # no defaults
                               "BMP files (*.bmp)|*.bmp",  # BMP only 
                               wx.OPEN);        # open a file
        if (dialog.ShowModal() == wx.ID_CANCEL):
            return  # canceled, bail
        
        # ok they opened a file...
        # turn into an Image

        im = wx.Image(dialog.GetPath())
        #print im.GetWidth(), im.GetHeight()
        self.filename = None
        self.SetTitle("pySpokePOV")
        self.wheel.setImage(im)

    def OnOpen(self, evt):
        global model
        dialog = wx.FileDialog(self,
                               "Choose an SpokePOV image to open...",  # title
                               "", "",          # no defaults
                               "BMP files (*.dat)|*.dat",  # DAT only
                               wx.OPEN);        # open a file
        if (dialog.ShowModal() == wx.ID_CANCEL):
            return
        self.filename = dialog.GetPath();
        self.SetTitle("pySpokePOV            "+self.filename)
        try:
            model = pickle.load(open(self.filename))
        except IOError:
            self.SetStatus("Error reading file!")
        model.set_changed(True)
        self.wheel.UpdateForegroundMask()
        self.wheel.Draw()

    def OnSave(self, evt):
        global model

        if (self.filename == None):
            dialog = wx.FileDialog(self,
                                   "Save SpokePov image...",  # title
                                   "", "",          # no defaults
                                   "Data file (*.dat)|*.dat",  # BMP only 
                                   wx.SAVE);        # open a file
            if (dialog.ShowModal() == wx.ID_CANCEL):
                return  # canceled, bail
            # ok they saved
            self.filename = dialog.GetPath();
            self.SetTitle("pySpokePOV            "+self.filename)

        try:
            f = open(self.filename, 'w')
            pickle.dump(model, f)
            f.close()
        except IOError:
            self.SetStatus("Error writing to file!")

            
    def OnSaveAs(self, evt):
        global model

        dialog = wx.FileDialog(self,
                               "Save SpokePov image as...",  # title
                               "", "",          # no defaults
                               "Data file (*.dat)|*.dat",  # BMP only 
                               wx.SAVE);        # open a file
        if (dialog.ShowModal() == wx.ID_CANCEL):
            return  # canceled, bail
        # ok they saved
        self.filename = dialog.GetPath();
        self.SetTitle("pySpokePOV            "+self.filename)
        
        try:
            f = open(self.filename, 'w')
            pickle.dump(model, f)
            f.close()
        except IOError:
            self.SetStatus("Error writing to file!")
        
    def OnTimeToClose(self, evt):
        """Event handler for the button click."""
        self.Close()

##############################################33
#  wrapper
####
class SpokeSoft(wx.App):
    def OnInit(self):
        frame = SpokeSoftFrame(None, "pySpokePOV")
        self.SetTopWindow(frame)

        frame.Show(True)
        return True
        
def main(argv=None):
    global config, hubsize, num_leds
    
    # read configuration
    try:
        config = ConfigParser.SafeConfigParser()
    except:
        #  Assume the problem is a python version older than 2.3
        #  (which is when SafeConfigParser got added)
        config = ConfigParser.ConfigParser()

    config.read("SpokePOV.cfg")
    try:
        # 4 LEDs per inch
        hubsize = float(config.get('preferences', 'hubsize')) * 4
    except ConfigParser.NoOptionError:
        hubsize = 4*1.5         # default is 2" diameter

    try:
        num_leds = int(config.get('preferences', 'num_leds'))
    except ConfigParser.NoOptionError:
        num_leds = 30

    app = SpokeSoft(redirect=False)

    if argv is None:
        argv = sys.argv
            
    app.MainLoop()

if __name__ == '__main__':
    # Redirect stdout/stderr
#    sys.stderr = open("stderr.log", "w")
#    sys.stdout = open("stdout.log", "w")

    sys.exit(main())


