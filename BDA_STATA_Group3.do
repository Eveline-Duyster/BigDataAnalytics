generate unempl_self=uempla+uempli
replace unempl_self = 1 if unempl_self==2

generate unempl_part=uemplap+uemplip
replace unempl_part = 1 if unempl_part==2

generate income = hinctnta
replace income = . if hinctnta==99
replace income = . if hinctnta==88
replace income = . if hinctnta==77

generate male = 1 if gndr==1
replace male = 0 if gndr==2

generate age = agea
replace age = . if agea==999

generate education = eisced
replace education = . if eisced==55
replace education = . if eisced==99
replace education = . if eisced==88
replace education = . if eisced==77

generate healt=health
replace healt=. if health==7
replace healt=. if health==8
replace healt=. if health==9

generate part_pres=1 if rshpsts==1
replace part_pres=1 if rshpsts==2
replace part_pres=1 if rshpsts==3
replace part_pres=1 if rshpsts==4
replace part_pres=0 if rshpsts==5
replace part_pres=0 if rshpsts==6
replace part_pres=0 if rshpsts==66

generate child=1 if chldhm==1
replace child=0 if chldhm==2

generate self_empl=1 if emplrel==2
replace self_empl=0 if emplrel==1
replace self_empl=0 if emplrel==3
replace self_empl=0 if emplrel==6

generate happy2=happy
replace happy2=. if happy==99
replace happy2=. if happy==88
replace happy2=. if happy==77

