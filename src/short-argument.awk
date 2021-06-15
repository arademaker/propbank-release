
BEGIN {OFS = "\t"}

$0 ~ /^[0-9]/ {
    for(i=11; i <= NF; i+=1){
	sub(/ARG/,"A",$i)
    }
    print; next
}

{print}
