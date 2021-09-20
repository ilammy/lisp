#lang pollen

◊section[#:label "escape/sect:handling"]{Формы, манипулирующие~продолжениями}

Явное использование продолжений позволяет управлять ходом исполнения программы.
Форма ◊ic{prog} имеет схожие возможности, но при этом обладает излишней функциональностью~◊ic{let}.
Оставив только управление потоком исполнения,
мы получим то, для чего (изначально) были придуманы формы ◊ic{catch} и~◊ic{throw}.
