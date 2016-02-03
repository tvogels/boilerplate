import random
import pymysql.cursors

def shuffle(features, labels):
  fl = zip(features,labels)
  random.shuffle(fl)
  features,labels = zip(*fl)
  return features, labels

def saveToSQL(featureset, C, max_iter, trsize, trscore, testscore):
  try:

    connection = pymysql.connect(host='localhost',
                                 user='root',
                                 password='',
                                 db='boilerplate',
                                 charset='utf8',
                                 cursorclass=pymysql.cursors.DictCursor)

    with connection.cursor() as cursor:
      # Create a new record
      sql = "INSERT INTO result (featureset,solver,solverOptions,edges,trainError,testError) VALUES (%s,'pystruct',%s,1,%s,%s)"
      cursor.execute(sql, (featureset, "C={0}, max_iter={1}, trsize={2}".format(C,max_iter,trsize),1-float(trscore),1-float(testscore)))
    connection.commit()
  finally:
    connection.close()