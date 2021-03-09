#!/usr/bin/env python3

def ten_to_bin(a):
    s = ""
    while (a > 0):
        s = str(a % 2) + s
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

    print("| MASK |" + '.'.join(mask) + " | " + '.'.join(ip_to_bin(mask)), "|")
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

tests =[
    ("1.1", ["168"]),
    ("1.1", ["10"]),
    ("1.1", ["255"]),
    ("1.1", ["2"]),
    ("1.2", ["209.165.200.229"]),
    ("1.2", ["172.16.18.183"]),
    ("1.2", ["10.86.252.17"]),
    ("1.2", ["255.255.255.128"]),
    ("1.2", ["255.255.192.0"]),
    ("2", ["172.16.145.29", "255.255.0.0"]),
    ("2", ["192.168.10.10", "255.255.255.0"]),
    ("2", ["192.168.68.210", "255.255.255.128"]),
    ("2", ["172.16.188.15", "255.255.240.0"]),
    ("2", ["10.172.2.8", "255.224.0.0"]),
    ("4", ["192.168.100.25", "28"]),
    ("4", ["172.30.10.130", "30"]),
    ("4", ["10.1.113.75", "19"]),
    ("4", ["198.133.219.250", "24"]),
    ("4", ["128.107.14.191", "22"]),
    ("4", ["172.16.104.99", "27"]),
    ("5", ["192.168.200.139", "255.255.255.0", "255.255.255.224"]),
    ("5", ["10.101.99.228", "255.0.0.0", "255.255.128.0"]),
    ("5", ["172.22.32.12", "255.255.0.0", "255.255.224.0"]),
    ("5", ["192.168.1.245", "255.255.255.0", "255.255.255.252"]),
    ("5", ["128.107.0.55", "255.255.0.0", "255.255.255.0"]),
    ("5", ["192.135.250.180", "255.255.255.0", "255.255.255.248"]),
]


# print("""
# Функции:
#   0-выход
#   1-перевод числа из 10 в 2
#   2- перевод ip из 10 в 2
#   3-рассчёт сетевого адреса
#   4-определение подсетей по IPv4-адресу
#   5-расчет подсетей по IPv4-адресу
#     """)

for ev, vals in tests:
    print(f"**{ev}**:")
    print("")
    print("--------")
    print("")
    print("| Значение | Десятичный | В двоичной |")
    if ev == "3":
        print("|--|--|")
    else:
        print("|--|--|--|")

    if ev == "1.1":
        print(f"| число | {vals[0]:<3} | {ten_to_bin(int(vals[0]))} |")

    if ev == "1.2":
        print(f"| ip | {vals[0]:<15} | ",
              '.'.join(ip_to_bin(vals[0].split('.'))), "|")

    if ev == "2":
        print("| ip   |        ", vals[0])
        ip = vals[0].split('.')
        print("| маску | ", vals[1])
        mask = vals[1].split('.')
        net_adr = net_by_mask(ip, mask)
        print("| IP | " + '.'.join(ip) + " | " +
              '.'.join(ip_to_bin(ip)), " |")
        print("| MASK |  " + '.'.join(mask) + " | " +
              '.'.join(ip_to_bin(mask)),  " |")

        print("| СЕТ. АДР. | " + '.'.join(net_adr) + " | " +
              '.'.join(ip_to_bin(net_adr)), " |")

    if ev == "3":
        pca = vals[0].split(".")
        pcb = vals[1].split(".")
        mask = vals[2].split(".")

        print(f"| PC A | {vals[0]} |")
        print(f"| PC B | {vals[1]} |")
        print(f"| MASK | {vals[2]} |")
        print("| сетевой адрес у PC-A |",
              '.'.join(net_by_mask(pca, mask)), " |")
        print("| сетевой адрес у PC-B |",
              '.'.join(net_by_mask(pcb, mask)), " |")

    if ev == "4":
        print("| ip | ", vals[0], " |", '.'.join(ip_to_bin(
            vals[0].split('.'))))
        ip = vals[0].split('.')
        print("| префикс |       ", vals[1], " |")
        pref = int(vals[1])
        mask = mask_by_pref(pref)
        net_adr = net_by_mask(ip, mask)
        broad_adr = broad_by_mask(ip, mask)

        print("| СЕТ. АДР. |" + '.'.join(net_adr) + " | " +
              '.'.join(ip_to_bin(net_adr)), "| |")
        print("| ШИР. АДР. | " + '.'.join(broad_adr), "|",
              '.'.join(ip_to_bin(broad_adr)), "|")
        print("| Количество бит узлов |" + str(32-pref), "| |")
        print("| Количество узлов | " + str(2**(32-pref) - 2), "| |")

    if ev == "5":
        print("| ip | ", vals[0], " |", '.'.join(ip_to_bin(
            vals[0].split('.'))))
        ip = vals[0].split('.')
        old_mask = vals[1].split('.')
        print("| исходную маску | ", vals[1], " |",
              '.'.join(ip_to_bin(old_mask)), "|")
        mask = vals[2].split('.')
        print("| новую маску | ", vals[2], " |",
              '.'.join(ip_to_bin(mask)), "|")
        old_pref = pref_by_mask(old_mask)
        pref = pref_by_mask(mask)

        print("| Количество бит подсети       |",
              str(pref - old_pref), " |")
        print("| Количество созданых подсетей |",
              str(2 ** (pref - old_pref)), " |")
        print("| Количество бит узлов подсети |",
              str(32 - pref), " |")
        print("| Количество узлов подсети     |",
              str(2**(32-pref) - 2), " |")

        net_adr = net_by_mask(ip,mask)
        print("| Сетевой адрес этой подсети |                  " +
              '.'.join(net_adr), " |",
              '.'.join(ip_to_bin(net_adr)), "|"
              )

        first_adr = first_by_net(net_adr)
        print("| адрес первого узла |      " +
              '.'.join(first_adr), " |",
              '.'.join(ip_to_bin(first_adr)), "|"
              )

        last_adr = last_by_broad(broad_adr)
        print("| адрес последнего узла |   " +
              '.'.join(last_adr), " |",
              '.'.join(ip_to_bin(last_adr)), "|"
              )


        broad_adr = broad_by_mask(ip, mask)
        print("| Широковещательный | " +
              '.'.join(broad_adr), " |",
              '.'.join(ip_to_bin(broad_adr)), "|"
              )


print("Done")
