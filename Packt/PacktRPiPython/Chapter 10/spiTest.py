#!/usr/bin/python3
# spiTest.py
import wiringpi2

print("Add SPI Loopback - connect P1-Pin19 and P1-Pin21")
print("[Press Enter to continue]")
input()
wiringpi2.wiringPiSPISetup(1,500000)
buffer=str.encode("HELLO")
print("Buffer sent %s" % buffer)
wiringpi2.wiringPiSPIDataRW(1,buffer)
print("Buffer received %s" % buffer)
print("Remove the SPI Loopback")
print("[Press Enter to continue]")
input()
buffer=str.encode("HELLO")
print("Buffer sent %s" % buffer)
wiringpi2.wiringPiSPIDataRW(1,buffer)
print("Buffer received %s" % buffer)
#End
