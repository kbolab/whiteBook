library(stringr)
library(pMineR)
library(markovchain)
library(stringr)

importer<-function() {
  imp.anag<-c();
  imp.dege<-c();
  imp.prest<-c();
  imp.file.f<-c();
  imp.ICD9<-c();
  imp.RT<-c();
  diz.reparti.ricovero<-c();
  dizionarioPrestazioni<-c();
  matrice.decod.reparto<-c();
  # -------------------------------------------------------------
  # load.csv
  # -------------------------------------------------------------  
  load.csv<-function( anag.fileName="ANAGRAFICA.txt", 
                      dege.fileName="DEGENZA_NEW.txt", 
                      prest.fileName="prestazioni.txt" , 
                      file.f.fileName='FILE_F.TXT', 
                      RT.fileName="monitoraggio_rt_conf.txt",
                      ICD9.fileName="ICD9.csv", 
                      default.folder='./import_01',
                      header = TRUE, sep="$") {

    
    dege.fileName <- file.path(default.folder,dege.fileName)
    file.f.fileName <- file.path(default.folder,file.f.fileName)
    ICD9.fileName <- file.path(default.folder,ICD9.fileName)
    RT.fileName <- file.path(default.folder,RT.fileName)
    
    cat("\n Inizio a caricare le degenze")
    imp.dege <<- read.csv(dege.fileName,header = header,sep = sep)
    post.processing.degenze()
    cat("\n Inizio a caricare le prestazioni")
    imp.prest <<- preprocessing.prestazioni(mainDir = default.folder, file2Process = prest.fileName, sep = sep, header = header)
    cat("\n Inizio a caricare i FILE F")
    imp.file.f <<- read.csv(file.f.fileName,header = header,sep = sep)   
    cat("\n Inizio a caricare gli ICD9")
    imp.ICD9 <<- read.csv(ICD9.fileName)
    cat("\n Inizio a caricare i dati della radioterapia")
    imp.RT <<- read.csv(file = RT.fileName ,header = T,sep = "\t")    

    # Sistema il codice sanitario (in una delle due tabelle)
    # il CODICE_SANITARIO vede aggiunto un carattere in fondo
#     cat("\n Aggiusto il codice Sanitario")
#     aggiusta.codice.sanitario();
    # E costruisci una tabella per associate l'ID al nome del reparto
    cat("\n Costruisco la matrice associativa")
    costruisci.matrice.associativa()
    # Aggiusta gli ICD9 aggiungendo una colonna numerica
#     cat("\n Aggiusto gli ICD9")
#     aggiusta.ICD9()
    cat("\n Dati importati.")
  }   
  # -------------------------------------------------------------
  # post.processing.degenze
  # -------------------------------------------------------------  
  post.processing.degenze<-function(){
    matrice.decod.reparto<<-cbind(as.character(imp.dege$REPARTO_RICOVERO),as.character(imp.dege$DESC_REPARTO_RICOVERO))
  }
  # -------------------------------------------------------------
  # preprocessing
  # -------------------------------------------------------------
  preprocessing.prestazioni<-function( mainDir, file2Process, tmp.dir = 'tmpDir', sep="$", header = TRUE) {
    
    dimensioni.file<-file.info(file.path(mainDir, file2Process))$size
    if(dimensioni.file > 400000001) {
    # if(dimensioni.file > 100001) {
      
      cat("\n\t Il file delle prestazioni supera i 400.000.000 byte.")
      # Crea la cartella se ancora non c'è
      if (!file.exists(file.path(mainDir, tmp.dir))) {
        dir.create(file.path(mainDir, tmp.dir))
      }
      if(file.exists(file.path(mainDir, tmp.dir,"big.tmp.imp.prest.RData"))) {
        cat("\n\t Ne ho trovato uno in cache, carico quello....")
        load(file.path(mainDir, tmp.dir,"big.tmp.imp.prest.RData"))
      }
      else {
        cat("\n\t cache vuota: lo calcolo")
        # Se i files non sono gia' stati creati
        if ( length(list.files(file.path(mainDir, tmp.dir)))==0 ) {
          # Prendo la prima riga (con gli header)
          prima.riga<-system2( "head", paste( c("-n 1 ",file.path(mainDir, file2Process)),collapse='')  ,stdout = TRUE)
          cat("\n\t Splitto i file...")
          stringa<-paste(   c("split -l 2000000 ",file.path(mainDir, file2Process), " ",  file.path(mainDir, tmp.dir,"new"))     , collapse='')
          # stringa<-paste(   c("split -l 300000 ",file.path(mainDir, file2Process), " ",  file.path(mainDir, tmp.dir,"new"))     , collapse='')
          system( stringa )
          lista.files.splittati <- list.files(file.path(mainDir, tmp.dir))
          system2( "head", paste( c("-n 1 ",file.path(mainDir, file2Process)," >> ",file.path(mainDir, tmp.dir,"header.txt")),collapse=''))
          for( nomeFileAppenaCreato in lista.files.splittati) {
            fullPath.nomeFileAppenaCreato <- file.path(mainDir, tmp.dir,nomeFileAppenaCreato)
            if(nomeFileAppenaCreato!="newaa") {
              system2( "cat", paste( c(file.path(mainDir, tmp.dir,"header.txt")," ",fullPath.nomeFileAppenaCreato ," > ", paste(c(fullPath.nomeFileAppenaCreato,".csv"),collapse='')    ),collapse = '' ) )
              file.remove(fullPath.nomeFileAppenaCreato)
            } else file.rename(from = fullPath.nomeFileAppenaCreato,to =   paste(c(fullPath.nomeFileAppenaCreato,".csv"),collapse='')    )
          }
        }
        if(file.exists(file.path(mainDir, tmp.dir,"header.txt"))) file.remove(file.path(mainDir, tmp.dir,"header.txt"))
        lista.files.splittati <- list.files(file.path(mainDir, tmp.dir))
        cat("\n\t Inizio a caricare i file splittati...")
        big.tmp.imp.dege<-c()
        for( nomeFile in lista.files.splittati) {
#           if(nomeFile=="newaa") header.file <- TRUE
#           else header.file <- FALSE
          fullPath.nomeFile <- file.path(mainDir, tmp.dir,nomeFile)
          cat("\n\t\t Loading Splitted file: ",fullPath.nomeFile)
#           tmp.imp.dege <- read.csv(fullPath.nomeFile,header = header.file,sep = sep)
#           tmp.imp.dege <- tmp.imp.dege[ ,1:18 ]
          tmp.imp.dege <- read.csv(fullPath.nomeFile,header = T,sep = sep)
#           if(nomeFile=="newaa.csv") nomiColonne<-names(tmp.imp.dege)
#           names(tmp.imp.dege)<-nomiColonne
          uppa<-as.data.frame(cbind(as.character(tmp.imp.dege$CODICE_SANITARIO_ADT),as.character(tmp.imp.dege$data_erog),as.character(tmp.imp.dege$AY_EXDES)))
          colnames(uppa)<-c("CODICE_SANITARIO_ADT", "data_erog", "AY_EXDES")
          uppa$CODICE_SANITARIO_ADT <- as.character(uppa$CODICE_SANITARIO_ADT)
          uppa$data_erog <- as.character(uppa$data_erog)
          uppa$AY_EXDES <- as.character(uppa$AY_EXDES)
          # uppa<-cbind(as.character(tmp.imp.dege$CODICE_SANITARIO_ADT),as.character(tmp.imp.dege$data_erog),as.character(tmp.imp.dege$AY_EXDES))
#           colnames(uppa)<-c("CODICE_SANITARIO_ADT", "data_erog", "AY_EXDES")
          big.tmp.imp.dege<-rbind(big.tmp.imp.dege,uppa)
          # colnames(big.tmp.imp.dege)<-c("CODICE_SANITARIO_ADT", "data_erog", "AY_EXDES")
        }
        cat("\n\t aggiorno la cache")
        save(big.tmp.imp.dege,file = file.path(mainDir, tmp.dir,"big.tmp.imp.prest.RData"))
        cat("\n\t cache aggiornata")
      }
      big.tmp.imp.dege<-as.data.frame(big.tmp.imp.dege)
    }
    else big.tmp.imp.dege <- read.csv(file.path(mainDir, file2Process),header = header,sep = sep) 
    big.tmp.imp.dege$CODICE_SANITARIO_ADT <- as.character(big.tmp.imp.dege$CODICE_SANITARIO_ADT)
    big.tmp.imp.dege$data_erog <- as.character(big.tmp.imp.dege$data_erog)
    big.tmp.imp.dege$AY_EXDES <- as.character(big.tmp.imp.dege$AY_EXDES)
    big.tmp.imp.dege$AA_ACSTA <- as.character(big.tmp.imp.dege$AA_ACSTA)
    big.tmp.imp.dege <- big.tmp.imp.dege[  which( big.tmp.imp.dege$AA_ACSTA == "7" | big.tmp.imp.dege$AA_ACSTA=="8" | big.tmp.imp.dege$AA_ACSTA=="9" ) ,]
    
    return(big.tmp.imp.dege)
  } 
  # -------------------------------------------------------------
  # aggiusta.codice.sanitario
  # -------------------------------------------------------------
  aggiusta.codice.sanitario<-function() {
    # Per le Prestazioni
    a<-imp.prest$CODICE_SANITARIO_ADT
    a<-unlist(lapply(a, function(x) { str_sub(x,1,str_length(x)-1)  }  ))
    imp.prest$CODICE_SANITARIO_ADT<<-a
    # per il FILE F
    a<-imp.file.f$codice_sanitario_adt
    a<-unlist(lapply(a, function(x) { str_sub(x,1,str_length(x)-1)  }  ))
    imp.file.f$codice_sanitario_adt<<-a    
  }
  # -------------------------------------------------------------
  # aggiusta.ICD9
  # -------------------------------------------------------------
  aggiusta.ICD9<-function() {
    colonnaValoriNumerici<-as.character(levels(imp.ICD9$icd9code ))
    imp.ICD9$numericValues<-colonnaValoriNumerici
  }  
  # -------------------------------------------------------------
  # get.lista.ICD9
  # restituisce la lista degli ICD9 trovati nel file
  # -------------------------------------------------------------  
  get.lista.ICD9<-function( ) {
    browser()
    s.1 <- as.character(   unique(imp.dege$DIAG1_ICD9CM)  )
    s.2 <- as.character(   unique(imp.dege$DIAG2_ICD9CM)  )
    s.3 <- as.character(   unique(imp.dege$DIAG3_ICD9CM)  )
    s.4 <- as.character(   unique(imp.dege$DIAG4_ICD9CM)  )
    s.5 <- as.character(   unique(imp.dege$DIAG5_ICD9CM)  )
    s.6 <- as.character(   unique(imp.dege$DIAG6_ICD9CM)  )
    s.7 <- as.character(   unique(imp.dege$DIAG7_ICD9CM)  )
    s.8 <- as.character(   unique(imp.dege$DIAG8_ICD9CM)  )
    
    return( unique( c(s.1,s.2,s.3,s.4,s.5,s.6,s.7,s.8 ))  )
  }  
  # -------------------------------------------------------------
  # get.campo.paziente.con.ICD9
  # estrae uno specifico campo dai pazienti che hanno almeno una diagnosi dell'ICD9 indicato
  #   ICD9 = la stringa contenente l'ICD9 da estrarre
  #   daRitornare = la stringa contenente il nome della colonna da ritornare
  #   unique.value = ?????
  # -------------------------------------------------------------
  # get.campo.paziente.con.ICD9<-function( ICD9 , daRitornare="CODICE_SANITARIO", unique.value = TRUE) {
  get.campo.paziente.con.ICD9<-function( ICD9 , daRitornare="CODICE_SANITARIO_ADT", unique.value = TRUE) {
    s.1 <- which(   imp.dege$DIAG1_ICD9CM == ICD9  )
    s.2 <- which(   imp.dege$DIAG2_ICD9CM == ICD9  )
    s.3 <- which(   imp.dege$DIAG3_ICD9CM == ICD9  )
    s.4 <- which(   imp.dege$DIAG4_ICD9CM == ICD9  )
    s.5 <- which(   imp.dege$DIAG5_ICD9CM == ICD9  )
    s.6 <- which(   imp.dege$DIAG6_ICD9CM == ICD9  )
    s.7 <- which(   imp.dege$DIAG7_ICD9CM == ICD9  )
    s.8 <- which(   imp.dege$DIAG8_ICD9CM == ICD9  )
    tot<- unique(c( s.1,s.2,s.3,s.4,s.5,s.6,s.7,s.8))
    if( unique.value == TRUE )
      return(   unique(imp.dege[[daRitornare]][ tot ])  )
    else 
      return(   imp.dege[[daRitornare]][ tot ]  )
  }  
  # -------------------------------------------------------------
  # prendi.prima.data.diagnosi.ICD9
  # estrae un set di campi relativi alla prima data di diagnosi dell'ICD9 indicato
  #   ICD9 = La stringa con l'ICD9 da estrarre
  #   daRitornare = un array di stringhe contenente i nomi dei campi da estrarre
  # -------------------------------------------------------------
  # prendi.prima.data.diagnosi.ICD9<-function( ICD9 , daRitornare=c("CODICE_SANITARIO","DATA_RICOVERO") ){
  prendi.prima.data.diagnosi.ICD9<-function( ICD9 , daRitornare=c("CODICE_SANITARIO_ADT","DATA_RICOVERO") ){    
    valori<-get.campo.paziente.con.ICD9(ICD9 = ICD9,unique.value = FALSE,daRitornare = "NOSOGRAFICO_ADT")
    
    if( length(table(valori))!=length(valori) ) {
      cat("\n ERRORE! Occhio, ci sono pazienti con due ricoveri con l'ICD9 indicato, devi modificare questa funzione perche' restituisca SOLO la prima data");
      return;
    }
    arr.elementi<- which(imp.dege[["NOSOGRAFICO_ADT"]] %in% valori)
    matriciona<-c()
    for(i in daRitornare) {
      matriciona<-cbind(matriciona, as.character(imp.dege[[ i ]][ arr.elementi  ]))
    }
    if(dim(matriciona)[1]==0) return();
    colnames(matriciona)<-daRitornare
    return(matriciona)
    
  }  
  # -------------------------------------------------------------
  # costruisci.matrice.associativa
  # restituisce una matrice che associa l'ID del reparto di ricovero e la relativa descrizione
  # -------------------------------------------------------------
  costruisci.matrice.associativa<-function( ID.colName = "REPARTO_RICOVERO", DESC.colName="DESC_REPARTO_RICOVERO"){
    b<-imp.dege$DESC_REPARTO_RICOVERO
    a<-imp.dege$REPARTO_RICOVERO
    df <-data.frame(a,b)
    
    matrice.reparti.ricovero<-df[!duplicated(df), ]  
    colnames(matrice.reparti.ricovero)<-c("ID","DESC")
    diz.reparti.ricovero<<-matrice.reparti.ricovero
  }  
  # -------------------------------------------------------------
  # caricaDizionario
  # carica un dizionario per il dettaglio delle prestazioni
  # -------------------------------------------------------------  
  caricaDizionario<-function( fileName = './dizionarioPrestazioni.csv' ) {
    # dizionarioPrestazioni <<- read.delim(fileName)
    dizionarioPrestazioni <<- read.csv(fileName,header = T,sep = ',')
  }  
  # -------------------------------------------------------------
  # estrai.submatrix.prestazioni.da.prima.diagnosi.ICD9
  # dal file delle prestazioni estrae tutte le prestazioni a partire dalla prima data
  # diagnosi dei pazienti con una diagnosi di ICD fra quelle passate nell'array. Utilizza anche CSV
  # del FILE F nel caso in cui ne sia stato caricato uno
  # arr.ICD9 = l'array degli ICD9 da estrarre
  # colonnaDizionario = la colonna con i raggruppamenti (nel dizionario)
  # matchingColumn.csv = la colonna del CSV che deve usare per il match (nel file delle prestazioni)
  # matchingColumn.diz = la colonna del dizionario da usare per il match
  # newColumnName = il nome della nuova colonna contenente il gruppo
  # misclassifiedName = stringa da usare nel caso di descrizioni che non trovano un raggruppamento
  # -------------------------------------------------------------
  estrai.submatrix.prestazioni.da.prima.diagnosi.ICD9<-function( arr.ICD9 = c("174.1","174.2"), colonnaDizionario=c(), matchingColumn.csv = "AY_EXDES", matchingColumn.diz = "EVENTO", newColumnName="GROUP_1", misclassifiedName = "MISCLASSIFIED" ) {
    matrice.date<-c()
    ct<-0
    # Estrai la sottomatrice delle prestazioni dei pazienti con gli ICD9 desiderati
    for( ICD9 in arr.ICD9 ) {
      matrice.date<-rbind(matrice.date,prendi.prima.data.diagnosi.ICD9( as.character(ICD9) , daRitornare=c("CODICE_SANITARIO_ADT","DATA_RICOVERO") ) )
      # matrice.date<-rbind(matrice.date, rep("01/01/1900",length(matrice.date)) )
    }
    
    # Costruisci l'array contenente i codici sanitari
    array.codici.sanitari<-unique(matrice.date[,1])
    
    # Per ogni riga aggiungi la data della prima diagnosi dell'ICD9 e il delta in 
    # giorni fra la diagnosi e la  data di erogazione della prestazione
    # a<-imp.prest
    # Fai un merging delle prestazioni e dei dati del FILE F
    b.1<-as.data.frame(cbind(as.character(imp.prest$CODICE_SANITARIO_ADT),as.character(imp.prest$data_erog),as.character(imp.prest$AY_EXDES)))
    b.2<-as.data.frame(cbind(as.character(imp.file.f$codice_sanitario_adt),as.character(imp.file.f$Data_erogazione),as.character(imp.file.f$desc_articolo)))
    # browser()
    colnames(b.1)<-c("CODICE_SANITARIO_ADT","data_erog","AY_EXDES")
    colnames(b.2)<-c("CODICE_SANITARIO_ADT","data_erog","AY_EXDES")
    a<-rbind(b.1,b.2)

    a$dataDiagnosi<-NA  
    # Cicla per ogni paziente (matrice.date è una tabella di due colonne <idpaziente,datadiagnosi>)
    for( riga in seq(1,nrow(matrice.date)) ) {
      # Trova le righe di imp.prest che hanno una corrispondenza con la riga di matrice.data in esame
      b<-which(a$CODICE_SANITARIO_ADT== matrice.date[riga,1])
      # se ce n'è almeno una
      if(length(b)>0) {
        # Aggiungi la data diagnosi ad imp.prest
        data.cazzuta <- "01/01/1970"
        a[b, "dataDiagnosi"]<-matrice.date[riga,2]
        # a[b, "dataDiagnosi"]<-data.cazzuta
        # Calcola il delta T
        deltaT<-as.numeric(difftime(as.POSIXct(a[b, "data_erog"], format = "%d/%m/%Y"),as.POSIXct(matrice.date[riga,2], format = "%d/%m/%Y"),units = 'days'))
        # deltaT<-as.numeric(difftime(as.POSIXct(a[b, "data_erog"], format = "%d/%m/%Y"),as.POSIXct(data.cazzuta, format = "%d/%m/%Y"),units = 'days'))
        # Aggiungi anche la colonna del DELTA T
        a[b, "delta.dataDiagnosi"]<- deltaT
      }
      cat("\n Now processing: ",matrice.date[riga,1])
      ct<-ct+1    
    }
    
    # togli i casi in cui la data diagnosi è NA e la prestazione è antecedente
    # alla data diagnosi   
    a<-a[!is.na(a$dataDiagnosi),]
    # a<- a[ which(a$delta.dataDiagnosi>=0)  ,]

    # Aggiungi i dati di Radioterapia (se ci sono)
    if( is.data.frame(imp.RT) ) {

      gg <- array.codici.sanitari[(array.codici.sanitari %in%  imp.RT$Codice.Sanitario)]
      RT.subset <- imp.RT[which(imp.RT$Codice.Sanitario %in% gg),]

      # sottotabellaPrestazioni<-cbind(RT.subset$Codice.Sanitario, as.character(RT.subset$Attività),as.character(RT.subset$Data.Effettuazione), rep(0,ncol(RT.subset)), rep(0,ncol(RT.subset)) )
      sottotabellaPrestazioni<-cbind(RT.subset$Codice.Sanitario, as.character(RT.subset$Data.Effettuazione), as.character(RT.subset[["Prestazione"]]), rep(0,nrow(RT.subset)), rep(0,nrow(RT.subset))  )   
      # colnames(sottotabellaPrestazioni)<-c("CODICE_SANITARIO_ADT","AY_EXDES","data_erog","dataDiagnosi","deltaDataDiagnosi")

      colnames(sottotabellaPrestazioni)<-c("CODICE_SANITARIO_ADT","data_erog","AY_EXDES","dataDiagnosi","delta.dataDiagnosi")
      for( codSan in gg) {
        b<-which(sottotabellaPrestazioni[,"CODICE_SANITARIO_ADT"]== codSan)
        dataDiagnosi <- a[which(a[,"CODICE_SANITARIO_ADT"]== codSan),"dataDiagnosi"][1]
        deltaT<- - as.numeric(difftime(as.POSIXct(dataDiagnosi, format = "%d/%m/%Y"),as.POSIXct(sottotabellaPrestazioni[b,"data_erog"], format = "%Y-%m-%d"),units = 'days'))        
        sottotabellaPrestazioni[ b , "delta.dataDiagnosi" ]<- deltaT
        sottotabellaPrestazioni[ b , "dataDiagnosi" ]<- dataDiagnosi 
      }
      a <- rbind(a,sottotabellaPrestazioni)
    }
    
    # Aggiungi i dati di Ricovero
    sottotabellaPrestazioni<-c()
    for( codSan in array.codici.sanitari) {
      cat("\n Ricovero, paz:",codSan)
      arr.str.date.usc<-list()
      arr.str.date.ing<-list()
      arr.str.rep<-list()
      dataDiagnosi <- a[which(a[,"CODICE_SANITARIO_ADT"]== codSan),"dataDiagnosi"][1]
      arr.riga <- which(imp.dege[["CODICE_SANITARIO_ADT"]]==codSan)
      
      for(riga in arr.riga) {
        sottotabellaPrestazioni<-c()
        for(i in seq(1,15)) { 
          data.rilevata.usc<-as.character(imp.dege[[str_c("DATAUSC",i)]][riga] )
          data.rilevata.ing<-as.character(imp.dege[[str_c("DATAING",i)]][riga] )
          reparto.ricovero<-as.character(imp.dege[[str_c("REP",i)]][riga] )  
          if(!is.na(data.rilevata.usc) & data.rilevata.usc!="") {
            arr.str.date.usc[[str_c("DATAUSC",i)]]<-data.rilevata.usc
          }
          if(!is.na(data.rilevata.ing) & data.rilevata.ing!="") {
            arr.str.date.ing[[str_c("DATAING",i)]]<-data.rilevata.ing
          }  
          if(!is.na(reparto.ricovero) & reparto.ricovero!="" & reparto.ricovero!="0") {
            desc.reparto.ricovero <- unique(matrice.decod.reparto[which(matrice.decod.reparto[,1]==reparto.ricovero),2])
            # Nella speranza che sia già stata censita!
            if(length(desc.reparto.ricovero)>0)
              arr.str.rep[[str_c("REP",i)]]<-desc.reparto.ricovero
            else
              arr.str.rep[[str_c("REP",i)]]<-reparto.ricovero
          }          
        }
        for(i in seq(1,length(arr.str.rep))) {
          deltaT.ric<- - as.numeric(difftime(as.POSIXct(dataDiagnosi, format = "%d/%m/%Y"),as.POSIXct(arr.str.date.ing[[i]], format = "%d/%m/%Y"),units = 'days'))        
          deltaT.usc<- - as.numeric(difftime(as.POSIXct(dataDiagnosi, format = "%d/%m/%Y"),as.POSIXct(arr.str.date.usc[[i]], format = "%d/%m/%Y"),units = 'days'))        
          sottotabellaPrestazioni <- rbind(sottotabellaPrestazioni, c(codSan,arr.str.date.ing[[i]],str_c("Ricovero ",arr.str.rep[i])   ,dataDiagnosi,deltaT.ric) )
          sottotabellaPrestazioni <- rbind(sottotabellaPrestazioni, c(codSan,arr.str.date.usc[[i]],str_c("Dimissione ",arr.str.rep[i])   ,dataDiagnosi,deltaT.usc) )
        }
        colnames(sottotabellaPrestazioni)<-c("CODICE_SANITARIO_ADT","data_erog","AY_EXDES","dataDiagnosi","delta.dataDiagnosi")
        a<-rbind(a,as.data.frame(sottotabellaPrestazioni))
      }
    }
    
    
    # Se era stato indicato un dizionario (e la relativa colonna) caricalo
    # e popola una colonna aggiuntiva
    if(length(colonnaDizionario)  > 0 ) {
      if(nrow(dizionarioPrestazioni)  > 0 ) {
        aaa<-as.character(a[[matchingColumn.csv]])
        bbb<-unlist(lapply(aaa, function(x) { 
          arrPosizioniTMP<-which(dizionarioPrestazioni[[ matchingColumn.diz ]]==x )
          if(length(arrPosizioniTMP)==0) return( misclassifiedName )
          else return(as.character(dizionarioPrestazioni[  arrPosizioniTMP  ,colonnaDizionario])  )
        }  ))
        a<-cbind(a,bbb)
        colnames(a)<-c(colnames(a)[1:(length(colnames(a))-1)] ,newColumnName)
      } 
    }
    return(a)
  }  
  # -------------------------------------------------------------
  # fa il merging delle prestazioni e dei file F creando una matrice delle prestazioni 
  # fittizia composta dalla somma delle due
  # -------------------------------------------------------------  
  merge.csv.to.built.bigTable<-function( ) {
    first.newline<-nrow(imp.prest)+1
    browser()
    stop("Not available yet!")
  }   
  # -------------------------------------------------------------
  # estrai.submatrix.ricoveri.da.prima.diagnosi.ICD9
  # dal file dei ricoveri estrae tutte le prestazioni a partire dalla prima data
  # diagnosi dei pazienti con una diagnosi di ICD fra quelle passate nell'array
  # -------------------------------------------------------------
  estrai.submatrix.ricoveri.da.prima.diagnosi.ICD9<-function( arr.ICD9 = c("174.1","174.2") ) {
    stop("Not available yet!")
  }   
  # -------------------------------------------------------------
  # importer
  # costruttore
  # -------------------------------------------------------------  
  importer<-function() {
    imp.anag<<-c();
    imp.dege<<-c();
    imp.prest<<-c();    
    imp.file.f<<-c();
    imp.ICD9<<-c();
    imp.RT<<-c();
    diz.reparti.ricovero<<-c()
    dizionarioPrestazioni<<-c()
    matrice.decod.reparto<<-c()
  }
  importer()
  return(
    list(
      "load.csv"=load.csv,
      "get.campo.paziente.con.ICD9"=get.campo.paziente.con.ICD9,
      "prendi.prima.data.diagnosi.ICD9"=prendi.prima.data.diagnosi.ICD9,
      "caricaDizionario"=caricaDizionario,
      "estrai.submatrix.prestazioni.da.prima.diagnosi.ICD9"=estrai.submatrix.prestazioni.da.prima.diagnosi.ICD9,
      "estrai.submatrix.ricoveri.da.prima.diagnosi.ICD9"=estrai.submatrix.ricoveri.da.prima.diagnosi.ICD9,
      "get.lista.ICD9"=get.lista.ICD9
      )
  )
}
# 
# timeDistribution.stats.plot<-function(res.dataLoader, 
#                            lst.select.attr.Name=NA, lst.select.attr.value=NA, 
#                            lst.pnt.attr.name=NA, lst.pnt.attr.value=NA, 
#                            color='red', plotIt=TRUE, plotGraph=TRUE, deltaDate.column.name='pMineR.deltaDate') { 
#   # Ordina per data diangosi
#   max.Delta<-c()
#   arr.occorrenze<-c(); occorrenza.cum<-c(); occorrenza.diff<-c();
#   aaa<-res.dataLoader$pat.process
#   
#   if(!(deltaDate.column.name %in% colnames(aaa[[1]])   )) stop(" please check the delta.date column name! ErrCode: #rh4389hcv ");
#   
#   for(i in seq(1,length(aaa) )) {
#     # aaa[[i]][order(aaa[[i]]$delta.dataDiagnosi),]
#     max.Delta<- c(max.Delta, max(aaa[[i]][[deltaDate.column.name]]))
#   } 
#   
#   # plotta gli assi e definisci i gap per le timeline
#   y.gap<-1;  x.gap<-20
#   if(plotIt==T) plot(0,0,xlim=c(0,max(max.Delta)+x.gap),ylim=c(0,length(aaa)*y.gap   ), ylab='Patients', xlab='Time',main='Patient\'s Timeline'  )
#   
#   # Cicla per ogni paziente
#   for(i in seq(1,length(aaa) )) {
#     # Array con i delta giorni di tutti gli eventi
#     arr.tak<-aaa[[i]]$delta.dataDiagnosi
#     # la riga orizzontale
#     if(plotIt==T) points(  x=c(0, max(arr.tak) ), y=c(i * y.gap,i *y.gap),  type='l' , col='grey' ) 
#     # le righette verticali
#     if(plotIt==T) points(  x=arr.tak, y=rep(c(i * y.gap),length(arr.tak) ) ,pch=3 , col='grey'  ) 
#     # passiamo ai colori
#     
#     for( indice in seq(1,length(lst.pnt.attr.name))) {
#       sottoMatrice<-aaa[[i]][ which(aaa[[i]][[ lst.pnt.attr.name[indice] ]] %in% lst.pnt.attr.value[[ lst.pnt.attr.name[indice] ]]  ) , ]
#       arr.tak.sottoMatrice<-sottoMatrice$delta.dataDiagnosi
#       if(plotIt==T) points(  x=arr.tak.sottoMatrice, y=rep(c(i * y.gap),length(arr.tak.sottoMatrice) ) ,pch=20, col=color  ) 
#       arr.occorrenze<-c(arr.occorrenze,arr.tak.sottoMatrice)
#     }
#   }
#   # Calcola l'occorrenza in cumulativo
#   arr.occorrenze<-unlist(arr.occorrenze)
#   for(i in seq(1,max(arr.occorrenze)) ) {
#     occorrenza.cum<-c(occorrenza.cum,length(arr.occorrenze[ which(arr.occorrenze<=i) ])	)
#     if(i>1) occorrenza.diff<-c(occorrenza.diff,occorrenza.cum[i]-occorrenza.cum[i-1])
#     else occorrenza.diff<-c(occorrenza.diff,occorrenza.cum)
#   } 
#   
#   if(plotGraph==TRUE){
#     plot(x = seq(1,length(occorrenza.diff)),y = occorrenza.diff ,type='l',col='red',lty=4, xlab='Time', ylab='Absolute Frequencies',main='Frequencies vs Time')
#     par(new=TRUE)
#     plot(x = seq(1,length(occorrenza.cum)),y = occorrenza.cum ,type='l',yaxt="n", col ='blue',lwd=2, xlab='', ylab='')
#     axis(4)
#     mtext("Cumulative Frequencies",side=4)
#   }
#   
#   return(list("arr.occorrenze"=arr.occorrenze,"occorrenza.cum"=occorrenza.cum,"occorrenza.diff"=occorrenza.diff))
# } 
# 
# 
# obj<-importer()
# obj$load.csv()
# obj$caricaDizionario()
# aaa<-obj$estrai.submatrix.prestazioni.da.prima.diagnosi.ICD9(arr.ICD9 = c("174.1","174.2","174.3","174.4","174.5","174.6","174.7","174.8","154.1","154.2","154.3","154.4","154.5","154.6","154.7","154.8"),colonnaDizionario = c("GROUP_1"),newColumnName = "GRUPPO")
# 
# obj.L<-dataLoader(); 
# obj.L$load.data.frame(mydata=aaa, IDName = "CODICE_SANITARIO_ADT",EVENTName = "GRUPPO",
#                       dateColumnName = "data_erog")
# obj.clEM<- cluster_expectationMaximization()
# 
# obj.clEM$loadDataset( obj.L$getData() ) 
# obj.clEM$calculateClusters(num = 2,typeOfModel = "firstOrderMarkovModel")
# uppa<-obj.clEM$getClusters() 
# 
# FOMM.1<-firstOrderMarkovModel()
# FOMM.2<-firstOrderMarkovModel()
# 
# FOMM.1$loadDataset(dataList = list("MMatrix"=uppa$clusters[[1]]))
# FOMM.2$loadDataset(dataList = list("MMatrix"=uppa$clusters[[2]]))
# 
# FOMM.1$trainModel()
# FOMM.2$trainModel()
# 
# 
# 
# kok<- logInspector()
# kok$loadDataset(obj.L$getData() )
# kok$timeDistribution.stats.plot(lst.pnt.attr.name = c("GRUPPO"),lst.pnt.attr.value = list("GRUPPO"=c("CHIRURGIA")),plotIt = TRUE,color = 'blue' )
# kok$timeDistribution.stats.plot(lst.pnt.attr.name = c("GRUPPO"),lst.pnt.attr.value = list("GRUPPO"=c("CHEMIO")),plotIt = TRUE )
# 
