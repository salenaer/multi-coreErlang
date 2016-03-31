for ((i=28; i>=6 ; i--));
do
dat=`date -v-${i}d -j "+%Y%m%d"` 
echo $dat
done