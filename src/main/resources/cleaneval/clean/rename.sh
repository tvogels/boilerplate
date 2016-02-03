for FILE in `ls *-cleaned.txt`
do
  if [[ $FILE =~ ^([0-9]+)-cleaned.txt$ ]];
  then
    mv $FILE ${BASH_REMATCH[1]}.txt
  fi
done