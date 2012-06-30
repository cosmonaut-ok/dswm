                                                                     
                                                                     
                                                                     
                                             
#!/bin/zsh

emulate -L zsh

local input_num index vol new_vol cur found voln setcmd

if [[ -z $1 ]] ; then
    print "Usage: pavol <input-number> [[+|-]<volume percent>"
    return 0
fi

input_num=$1
new_vol=$2

index=
vol=
cur=1

if [[ $input_num != master ]] ; then
    pacmd list-sink-inputs | while read one two three ; do
        case $one ; in
            (index:)
                index=$two
                ;;
            (volume:)
                vol=${three%%%*}
                ;;
            (application.name)
                if [[ $three == *${input_num}* ]] ; then
                    found=1
                    break
                fi
            ;;
        esac
    done
    setcmd=set-sink-input-volume
else
    pacmd list-sinks | while read one two three four ; do
        case $one ; in
            (volume:)
                vol=${three%*%}
                ;;
        esac
        if [[ -n $vol ]] ; then
            found=1
            break
        fi
    done
    setcmd=set-sink-volume
    index=0
fi

if [[ $found == 1 ]] ; then
    integer voln

    (( voln = vol*(65535/100)))
    if [[ -n $new_vol ]] ; then
        if [[ $new_vol[1] == '+' ]] ; then
            (( voln = voln + (new_vol[2,-1] * 65536/100)))
        elif [[ $new_vol[1] == '-' ]] ; then
            (( voln = voln - (new_vol[2,-1] * 65536/100)))
        else
            (( voln = new_vol * (65536/100)))
        fi
        if [[ $voln -gt 65535 ]] ; then
            voln=65535
        fi
        if [[ $voln -lt 0 ]] ; then
            voln=0
        fi
        print "Running pacmd $setcmd $index $voln"
        pacmd $setcmd $index $voln >/dev/null 2>&1
    fi
    print "$vol"
else
    print "Not found"
fi

