#!/usr/bin/env python3

def ten_to_bin(a):
    s = ""
    while (a > 0):
        s = str(a%2) + s
        a = int(a / 2)
    return s

def bin_to_ten (a):
    n = 0
    for i in range(len(a)):
        if a[i] == "1":
            n += 2**(len(a) - 1- i)
    return n

def ip_to_ten (A):
    S = []
    for i in range(4):
        s = bin_to_ten(A[i])
        S.append(str(s))
    return S

def ip_to_bin (A):
    S = []
    for i in range(4):
        s = ten_to_bin(int(A[i]))
        S.append("0"*(8-len(s))+s)
    return S

def mask_by_pref(p):
    mask = []
    while p >= 8:
        mask.append("255")
        p = p - 8
    if p != 0:
        n = 0
        for i in range(p):
            n += 2**(7-i)
        mask.append(str(n))

    for i in range(4 - len(mask)):
        mask.append("0")

    print("MASK: " + '.'.join(mask) + " в двоичной: " + '.'.join(ip_to_bin(mask)))
    return mask

def net_by_mask (IP, MASK):
    NET = []
    for i in range(4):
            NET.append(str(int(IP[i]) & int(MASK[i])))
    return NET

def broad_by_mask (IP, MASK):
    ip = ip_to_bin(IP)
    mask = ip_to_bin(MASK)
    brd = []
    for i in range(4):
        s = ""
        for j in range(8):
            if mask[i][j] == "0":
                s += "1"
            elif ip[i][j] == "1":
                s += "1"
            else:
                s += "0"
        brd.append(s)
    return ip_to_ten(brd)

def pref_by_mask (MASK):
    pref = 0
    for i in range(4):
        if MASK[i] == "255":
            pref += 8
        else:
            st = ten_to_bin(int(MASK[i]))
            for j in range(len(st)):
                if st[j] == "1":
                    pref += 1
            return pref

def first_by_net (NET):
    first = NET
    first[3] = str( int(first[3]) + 1)
    return first

def last_by_broad (BRD):
    last = BRD
    last[3] = str( int(last[3]) - 1)
    return last

ev = 1
net_adr = []


while (ev != 0):
    print("\nФункции:\n0-выход\n1-перевод числа из 10 в 2\n2- перевод ip из 10 в 2\n3-рассчёт сетевого адреса")
    print("4-определение подсетей по IPv4-адресу\n5-расчет подсетей по IPv4-адресу")
    ev = int(input())
    if ev == 1:

        print("Введите число: ")
        print(ten_to_bin(int(input())))

    if ev == 2:

        print("Введите ip через точку: ")
        print(  '.'.join(ip_to_bin(input().split('.'))) )

    if ev == 3:

        print("Введите ip через точку: ")
        ip = input().split('.')
        print("Введите маску через точку: ")
        mask = input().split('.')
        net_adr = net_by_mask(ip, mask)
        print("\nIP: " + '.'.join(ip) + " в двоичной: " + '.'.join(ip_to_bin(ip)))
        print("MASK: " + '.'.join(mask) + " в двоичной: " + '.'.join(ip_to_bin(mask)))
        print("СЕТ. АДР.: " + '.'.join(net_adr) + " в двоичной: " + '.'.join(ip_to_bin(net_adr)))


    if ev == 4:
        print("Введите ip через точку: ")
        ip = input().split('.')
        print("Введите префикс: ")
        pref = int(input())
        mask = mask_by_pref(pref)
        net_adr = net_by_mask(ip, mask)
        broad_adr = broad_by_mask(ip, mask)

        print("СЕТ. АДР.: " + '.'.join(net_adr) + " в двоичной: " + '.'.join(ip_to_bin(net_adr)))
        print("ШИР. АДР.: " + '.'.join(broad_adr))
        print("\nКоличество бит узлов: " + str(32-pref))
        print("Количество узлов: " + str(2**(32-pref) - 2))

    if ev == 5:
        print("Введите ip через точку: ")
        ip = input().split('.')
        print("Введите исходную маску через точку: ")
        old_mask = input().split('.')
        print("Введите новую маску через точку: ")
        mask = input().split('.')
        old_pref = pref_by_mask(old_mask)
        pref = pref_by_mask(mask)

        print("\nКоличество бит подсети: " + str(pref - old_pref))
        print("Количество созданых подсетей: " + str(2 ** (pref - old_pref)))
        print("Количество бит узлов подсети: " + str(32 - pref))
        print("Количество узлов подсети: " + str(2**(32-pref) - 2))

        net_adr = net_by_mask(ip,mask)
        print("\nСетевой адрес этой подсети: " + '.'.join(net_adr))
        first_adr = first_by_net(net_adr)
        print("IPv4-адрес первого узла в этой подсети: " + '.'.join(first_adr))
        broad_adr = broad_by_mask(ip, mask)
        print("Широковещательный IPv4-адрес в этой подсети: " + '.'.join(broad_adr))
        last_adr = last_by_broad(broad_adr)
        print("IPv4-адрес последнего узла в этой подсети: " + '.'.join(last_adr))
