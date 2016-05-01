#!/usr/bin/python3
#keypassing.py
import encryptdecrypt as ENC

KEY1 = 20
KEY2 = 50

print ("Please enter text to scramble:")
#Get user input
user_input = input()
#Send message out
encodedKEY1 = ENC.encryptText(user_input,KEY1)
print ("USER1: Send message encrypted with KEY1 (KEY1): " +
       encodedKEY1)
#Receiver encrypts the message again
encodedKEY1KEY2 = ENC.encryptText(encodedKEY1,KEY2)
print ("USER2: Encrypt with KEY2 & returns it (KEY1+KEY2): " +
       encodedKEY1KEY2)
#Remove the original encoding
encodedKEY2 = ENC.encryptText(encodedKEY1KEY2,-KEY1)
print ("USER1: Removes KEY1 & returns with just KEY2 (KEY2): " +
       encodedKEY2)
#Receiver removes their encryption
message_result = ENC.encryptText(encodedKEY2,-KEY2)
print ("USER2: Removes KEY2 & Message received: " + message_result)
#End
