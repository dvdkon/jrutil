// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2020 David Koňařík

module JrUtil.Holidays

open NodaTime

let easterSundayDate year =
    // Algorithm taken from "New Scientist" referenced by Wikipedia
    // I'm pretty sure this only works by accident
    let a = year % 19
    let b = year / 100
    let c = year % 100
    let d = b / 4
    let e = b % 4
    let g = (8*b + 13) / 25
    let h = (19*a + b - d - g + 15) % 30
    let i = c / 4
    let k = c % 4
    let l = (2*e + 2*i - h - k + 32) % 7
    let m = (a + 11*h + 19*l) / 433
    let n = (h + l - 7*m + 90) / 25
    let p = (h + l - 7*m + 33*n + 19) % 32
    p, n

/// List of state holiday dates in (day, month) tuples
let czechHolidays year =
    let easterDay, easterMonth = easterSundayDate year
    let easterDaysSinceMarch = easterDay + (easterMonth - 3)*31
    let goodFridayDSM = easterDaysSinceMarch - 2
    let easterMondayDSM = easterDaysSinceMarch + 1
    [
        1, 1
        8, 5
        5, 7
        6, 7
        28, 9
        28, 10
        17, 11
        1, 5
        24, 12
        25, 12
        26, 12
        goodFridayDSM % 31, goodFridayDSM / 31 + 3
        easterMondayDSM % 31, easterMondayDSM / 31 + 3
    ]