
source("importer.R")

# ----------------------------------------------------
# IMPORTER
# ----------------------------------------------------
# Estraggo i dati con l'importer
# ----------------------------------------------------
# Creo un oggetto IMPORTER, per caricare i dati del datawarehouse
obj<-importer()
# Carico i CSV, indicandogli dove sono ed i nomi dei files
obj$load.csv(anag.fileName="ANAGRAFICA.txt", 
             dege.fileName="DEGENZA_NEW.txt", 
             prest.fileName="prestazioni.txt" , 
             file.f.fileName='FILE_F.TXT', 
             ICD9.fileName="ICD9.csv", 
             default.folder='../otherFiles/import_01',
             header = TRUE, sep="$")
# Carico il dizionario (per il grouping delle prestazioni)
obj$caricaDizionario( fileName = './dizionarioPrestazioni.csv' )

# Estrai i dati dei pazienti trattati per ca mammario
# (Notare la scelta del tipo di raggruppamento, indicato dalla colonna del file 'dizionarioPrestazioni.csv')

mammella<-obj$estrai.submatrix.prestazioni.da.prima.diagnosi.ICD9(
  arr.ICD9 = c("174.0","174.1","174.2","174.3","174.4","174.5","174.6","174.7","174.8","174.9"),
  colonnaDizionario = c("GROUP_2"),newColumnName = "GRUPPO")

# Estrai i dati dei pazienti trattati pe ca retto
retto<-obj$estrai.submatrix.prestazioni.da.prima.diagnosi.ICD9(
  arr.ICD9 = c("154.0","154.1","154.2","154.3","154.4","154.5","154.6","154.7","154.8","154.9"),
  colonnaDizionario = c("GROUP_2"),newColumnName = "GRUPPO")


# ----------------------------------------------------
# DATALOADER
# ----------------------------------------------------
# Passo i dati estratti in un dataLoader
# (non caricherà da CSV, ma carica ed elabora prendendo in ingresso un dataframe)
# ----------------------------------------------------
# Ok, ora crea un oggetto DATALOADER
obj.L.mammella<-dataLoader(); 
obj.L.retto<-dataLoader(); 
obj.L.mammella$load.data.frame(mydata=mammella, IDName = "CODICE_SANITARIO_ADT",EVENTName = "GRUPPO",
                      dateColumnName = "data_erog")
obj.L.retto$load.data.frame(mydata=retto, IDName = "CODICE_SANITARIO_ADT",EVENTName = "GRUPPO",
                               dateColumnName = "data_erog")

# Rimuovi alcuni eventi (sono stati raggruppati) che non interessano
arr.da.togliere<-c("AMMINISTRATIVO","PRELIEVO","MEDICAZIONE","ALTRO","AGOPUNTURA","MISCLASSIFIED")

obj.L.mammella$removeEvents(array.events = arr.da.togliere)
obj.L.retto$removeEvents(array.events = arr.da.togliere)

# Applica un filtro: togli tutte le prestazioni che compaiono meno di 3 volte in tutto
# (sono rumore che potrebbe creare delle alterazioni nella matrice di transizione 
# inducendo delle probabilità '1' in qualche transizione)
obj.L.retto$apply.filter(filter.list = list("event.absolute.coverage.threshold"=list("threshold"=3)))
obj.L.mammella$apply.filter(filter.list = list("event.absolute.coverage.threshold"=list("threshold"=3)))


# ----------------------------------------------------
# LOG INSPECTOR
# ----------------------------------------------------
# plottiamo un po' di roba
# ----------------------------------------------------
# Crea l'oggetto e carica i dati'
kok.mammella<- logInspector()
kok.mammella$loadDataset(obj.L.mammella$getData() )
# Mostra i due grafici della CHIRURGIA nel tempo
kok.mammella$timeDistribution.stats.plot(lst.pnt.attr.name = c("GRUPPO"),lst.pnt.attr.value = list("GRUPPO"=c("CHIRURGIA")),color = 'blue' )
# ed i due grafici della CHEMIO
kok.mammella$timeDistribution.stats.plot(lst.pnt.attr.name = c("GRUPPO"),lst.pnt.attr.value = list("GRUPPO"=c("CHEMIO")))

# Fai lo stesso per il ca del retto
kok.retto<- logInspector()
kok.retto$loadDataset(obj.L.retto$getData() )
kok.retto$timeDistribution.stats.plot(lst.pnt.attr.name = c("GRUPPO"),lst.pnt.attr.value = list("GRUPPO"=c("CHIRURGIA")),color = 'blue' )
kok.retto$timeDistribution.stats.plot(lst.pnt.attr.name = c("GRUPPO"),lst.pnt.attr.value = list("GRUPPO"=c("CHEMIO")) )


# ----------------------------------------------------
# FIRST ORDER MARKOV MODEL
# ----------------------------------------------------
# plottiamo un po' di roba
# ----------------------------------------------------
# creiamo due modelli con una threshold di probabilità pari a .01
# (le transizioni con probabilità inferiore verranno settate a 0)
FOMM.mammella<-firstOrderMarkovModel(parameters.list = list("threshold"=0.1))
FOMM.retto<-firstOrderMarkovModel(parameters.list = list("threshold"=0.1))

FOMM.mammella$loadDataset(obj.L.mammella$getData())
FOMM.retto$loadDataset(obj.L.retto$getData())

FOMM.mammella$trainModel()
FOMM.retto$trainModel()

FOMM.mammella$plot()
FOMM.retto$plot()

# Plotta la differenza fra i due grafici, secondo la nuova sintassi
# proposta dal prof. VALENTINI
FOMM.mammella$plot.delta.graph(objToCheck = FOMM.retto,threshold =  .1,type.of.graph = "overlapped",threshold.4.overlapped = .25)

# ----------------------------------------------------
# CLUSTERING
# ----------------------------------------------------
# Passiamo ora ad un po' di clustering
# ----------------------------------------------------
# Creiamo l'oggeto
obj.clEM<- cluster_expectationMaximization()
# Carichiamo i dati presi dal dataLoader
obj.clEM$loadDataset( obj.L.retto$getData() )
# Clusterizziamo secondo il FOMM
obj.clEM$calculateClusters(num = 2,typeOfModel = "firstOrderMarkovModel")
# Recuperiamo i clusters: se non saranno due, le righe successive daranno errore !
uppa<-obj.clEM$getClusters() 

FOMM.1<-firstOrderMarkovModel(parameters.list = list("threshold"=0.1))
FOMM.2<-firstOrderMarkovModel(parameters.list = list("threshold"=0.1))

FOMM.1$loadDataset(dataList = list("MMatrix"=uppa$clusters[[1]]))
FOMM.2$loadDataset(dataList = list("MMatrix"=uppa$clusters[[2]]))
 
FOMM.1$trainModel()
FOMM.2$trainModel()

FOMM.1$plot()
FOMM.2$plot()

# Vediamo la differenza fra i due clusters! :)
FOMM.2$plot.delta.graph(objToCheck = FOMM.1,threshold =  .1,type.of.graph = "overlapped",threshold.4.overlapped = .25)

