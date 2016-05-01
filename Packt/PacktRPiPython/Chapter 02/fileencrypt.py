#!/usr/bin/python3
#fileencrypt.py
import sys #Imported to obtain command line arguments
import encryptdecrypt as ENC

#Define expected inputs
ARG_INFILE=1
ARG_OUTFILE=2
ARG_KEY=3
ARG_LENGTH=4

def covertFile(infile,outfile,key):
    #Convert the key text to an integer
    try:
        enc_key=int(key)
    except ValueError:
        print ("Error: The key %s should be an integer value!"
               % (key))
    else:
        try:
            #Open the files
            with open(infile) as f_in:
                infile_content=f_in.readlines()
        except IOError:
            print ("Unable to open %s" % (infile))
        try:
            with open(outfile,'w') as f_out:
                for line in infile_content:
                    out_line = ENC.encryptText(line,enc_key)
                    f_out.writelines(out_line)
        except IOError:
            print ("Unable to open %s" % (outfile))
        print ("Conversion complete: %s" % (outfile))
    finally:
        print ("Finish")

#Check the arguments
if len(sys.argv) == ARG_LENGTH:
    print ("Command: %s" %(sys.argv))
    covertFile(sys.argv[ARG_INFILE],
               sys.argv[ARG_OUTFILE],
               sys.argv[ARG_KEY])
else:
    print ("Usage: fileencrypt.py infile outfile key")
