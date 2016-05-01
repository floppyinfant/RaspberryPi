#!/usr/bin/python3
# matrixControl.py
import wiringpi2
import time

MAX7219_NOOP        = 0x00
DIG0=0x01; DIG1=0x02; DIG2=0x03; DIG3=0x04
DIG4=0x05; DIG5=0x06; DIG6=0x07; DIG7=0x08
MAX7219_DIGIT=[DIG0,DIG1,DIG2,DIG3,DIG4,DIG5,DIG6,DIG7]
MAX7219_DECODEMODE  = 0x09
MAX7219_INTENSITY   = 0x0A
MAX7219_SCANLIMIT   = 0x0B
MAX7219_SHUTDOWN    = 0x0C
MAX7219_DISPLAYTEST = 0x0F
SPI_CS=1
SPI_SPEED=100000

class matrix():
  def __init__(self,DEBUG=False):
    self.DEBUG=DEBUG
    wiringpi2.wiringPiSPISetup(SPI_CS,SPI_SPEED)
    self.sendCmd(MAX7219_SCANLIMIT, 8)   # enable outputs
    self.sendCmd(MAX7219_DECODEMODE, 0)  # no digit decode
    self.sendCmd(MAX7219_DISPLAYTEST, 0) # display test off
    self.clear()
    self.brightness(7)                   # brightness 0-15
    self.sendCmd(MAX7219_SHUTDOWN, 1)    # start display
  def sendCmd(self, register, data):
    buffer=(register<<8)+data
    buffer=buffer.to_bytes(2, byteorder='big')
    if self.DEBUG:print("Send byte: 0x%04x"%
                        int.from_bytes(buffer,'big'))
    wiringpi2.wiringPiSPIDataRW(SPI_CS,buffer)
    if self.DEBUG:print("Response:  0x%04x"%
                        int.from_bytes(buffer,'big'))
    return buffer
  def clear(self):
    if self.DEBUG:print("Clear")
    for row in MAX7219_DIGIT:
      self.sendCmd(row + 1, 0)
  def brightness(self,intensity):
    self.sendCmd(MAX7219_INTENSITY, intensity % 16)

def letterK(matrix):
    print("K")
    K=(0x0066763e1e366646).to_bytes(8, byteorder='big')
    for idx,value in enumerate(K):
        matrix.sendCmd(idx+1,value)

def main():
    myMatrix=matrix(DEBUG=True)
    letterK(myMatrix)
    while(1):
      time.sleep(5)
      myMatrix.clear()
      time.sleep(5)
      letterK(myMatrix)

if __name__ == '__main__':
    main()
#End
