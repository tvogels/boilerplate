for FILE in `ls *.txt.utf8`
do
  ENCODING=`uchardet $FILE`
  echo $FILE: $ENCODING
done