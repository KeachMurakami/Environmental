import csv
import serial

ser = serial.Serial(3) # COM4
smoothing = 100 # smoothing for calculating the ave and SD of the value

with open('ArduinoPFD.csv', 'w') as f:
  writer = csv.writer(f)
  a = [ser.read(smoothing)]
  writer.writerow(a)
  f.close()

print("csv wrote")
