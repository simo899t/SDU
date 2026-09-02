# -*- coding: utf-8 -*-
"""
QUERY 1
"""


import psycopg2

con_str = "host='localhost' port='5432' dbname='postgres' user='nomis' password='hejsa1234'"

try:
    conn = psycopg2.connect(con_str)
    print("Connection Succesfull!")
except:
    print("I am unable to connect to the database")
    
    
    

#user_price = int(input("Enter a price: "))
#min_diff = 1000000000
#closest_model = -1
#
#cur = conn.cursor()
#
#try:
#    cur.execute("""SELECT model, price FROM pc""")
#    
#except:
#    print("I can't SELECT from pc")
#
#rows = cur.fetchall()
#
#for row in rows:
#
#    curr_model = row[0]
#    curr_price = row[1]
#
#    if abs(user_price - curr_price) < min_diff:
#
#        closest_model = curr_model
#        min_diff = abs(user_price - curr_price)
#  
#print(closest_model, min_diff)
#
#
#
#
#"""
#QUERY 2
#
#import psycopg2
#
#con_str = "host='localhost' port='5432' dbname='postgres' user='postgres' password='123654789'"
#
#try:
#    conn = psycopg2.connect(con_str)
#    print("Connection Succesfull!")
#except:
#    print("I am unable to connect to the database")
#    
#min_speed = input("Enter a minimum speed: ")
#min_ram = input("Enter a minimum ram: ")
#min_hdd = input("Enter a minimum hdd size: ")
#min_screen = input("Enter a minimum screen size: ")
#
#cur = conn.cursor()
#
#try:
#    
#    #cur.execute("SELECT speed, ram, hd, screen, maker FROM laptop INNER JOIN product ON laptop.model = product.model WHERE speed > " + min_speed + "::double precision AND ram > " + min_ram + "::integer AND hd > " + min_hdd + "::integer AND screen > " + min_screen + "::double precision;")
#    cur.execute("SELECT speed, ram, hd, screen, maker FROM laptop INNER JOIN product ON laptop.model = product.model WHERE speed >  %s AND ram > %s AND hd > %s AND screen > %s;", (float(min_speed), int(min_ram), int(min_hdd), float(min_screen)))
#
#except:
#    
#    print("I can't SELECT from pc")
#
#rows = cur.fetchall()
#
#for row in rows:
#
#    print(row[0], row[1], row[2], row[3], row[4])
#"""