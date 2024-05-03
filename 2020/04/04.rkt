#lang racket/base

(require racket/file)
(require racket/list)
(require racket/match)
(require racket/string)

(define (prepare-pps str)
  (string-split (string-replace str " " "\n") "\n\n"))

(define test-data "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in")

(define valid (prepare-pps "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f

eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

hcl:#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022

iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"))

(define invalid (prepare-pps "eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946

hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007"))

(define (count-valid-passports str)
  (count valid-passport? (prepare-pps str)))

(define (count-valid-passports-2 str)
  (count valid-passport-2? (prepare-pps str)))

(define (valid-passport? passport)
  (let ([no-fields (length (string-split passport "\n"))])
    (cond [(<= no-fields 6) #f]
          [(= 7 no-fields)
           (not (regexp-match #rx"cid:[0-9]+" passport))] ;; if cid field present, something else is missing
          [else #t ])))

(define matches
  (make-hash (list (cons 'byr  (lambda (byr)
                                 (match byr
                                   [(regexp #px"^byr:([0-9]{4})$"
                                            (list _ (app string->number n)))
                                    (<= 1920 n 2002)]
                                   [_ #f])))
                   (cons 'cid  (lambda (_) #t))
                   (cons 'ecl  (lambda (ecl)
                                 (match ecl
                                   [(regexp #px"ecl:([a-z]{3})$" (list _ value))
                                    (member value '("amb" "blu" "brn" "gry" "grn" "hzl" "oth"))]
                                   [_ #f])))
                   (cons 'eyr  (lambda (eyr)
                                 (match eyr
                                   [(regexp #px"^eyr:([0-9]{4})$"
                                            (list _ (app string->number n)))
                                    (<= 2020 n 2030)]
                                   [_ #f])))
                   (cons 'hcl  (lambda (hcl)
                                 (regexp-match #px"^hcl:\\#[a-f0-9]{6}$" hcl)))
                   (cons 'hgt  (lambda (hgt)
                                 (match hgt
                                   [(regexp #px"^hgt:([0-9]+)in$"
                                            (list _ (app string->number inches)))
                                    (<= 59 inches 76)]
                                   [(regexp #px"^hgt:([0-9]+)cm$"
                                            (list _ (app string->number cm)))
                                    (<= 150 cm 193)]
                                   [_ #f])))
                   (cons 'iyr  (lambda (iyr)
                                 (match iyr
                                   [(regexp #px"^iyr:([0-9]{4})$"
                                            (list _ (app string->number n)))
                                    (<= 2010 n 2020)])))
                   (cons 'pid (lambda (pid)
                                (regexp-match #px"^pid:[0-9]{9}$" pid))))))

(define (valid-passport-2? passport)
  (and (valid-passport? passport)
       (for/and ([line (string-split passport "\n")])
         (let ([key (car (regexp-match #px"[a-z]{3}" line))])
           ((hash-ref matches (string->symbol key)) line)))))

(count-valid-passports (file->string "input.txt"))
(count-valid-passports-2 (file->string "input.txt"))

