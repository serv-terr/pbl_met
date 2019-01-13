# Il modello a particelle ALAMO V1.0

## Guida operativa

M. Favaron - Gen 2019

### Il modello ALAMO

#### Cos’è, a cosa serve

Il modello ALAMO (acronimo di **A**tmospheric **LA**grangian dispersion **MO**del) è, come suggerisce il nome, un modello di dispersione lagrangiano a particelle.

A differenza dei modelli a particelle di maggior diffusione, alimentati con un campo meteorologico tridimensionale, ALAMO utilizza una meteorologia “uni-dimensionale”, cioè variabile solo lungo la verticale (e, naturalmente, nel tempo).

La ragione dietro a questa differenza è l’enfasi, in ALAMO, data alla comprensione *locale* del trasporto e diffusione di traccianti passivi (gas, particolati sottili, l’atmosfera stessa, batteri, spore). Questa meteorologia uni-dimensionale si suppone misurata direttamente tramite ad esempio un SODAR (con o senza estensione RASS), oppure stimata a partire da misure dirette di turbolenza compiute vicino al suolo usando un anemometro ultrasonico tri-assiale come ad esempio l’**uSonic-3** di Metek GmbH ed un sistema di acquisizione ed elaborazione dedicato come ad esempio il **MeteoFlux® Core V2** di Servizi Territorio srl.

ALAMO non è progettato per applicazioni *regulatory*, e come tale *non* deve essere utilizzato. Il suo impiego è principalmente *tecnico*, di supporto al monitoraggio (per esempio, a supporto di campagne olfattometriche, misure di concentrazione al camino, formulazione di piani di monitoraggio su basi razionali).

#### Come si avvia

Per avviare ALAMO è sufficiente aprire una sessione terminale e digitare il comando

​            **alamo <File_Configurazione_Studio>**

dove **<File_Configurazione_Studio>** è il file di input principale, descritto nel seguito.

### Dati di ingresso

#### Il file di ingresso principale

##### Alcune cose da sapere

Il file di ingresso principale contiene direttamente, o tramite riferimenti, tutte le informazioni che servono per una girata ALAMO. Esistono, però, "girate" di tipo diverso (scelto appunto tramite opportune grandezze nel file di input principale), delle quali è necessario sapere prima di procedere.

La prima cosa da sapere è, che il modello ALAMO può produrre output di vari tipi, ma anche, volendo, _nessun_ output. E' infatti ammessa la possibilità di predisporre una girata "di prova", nella quale l'elaborazione procede "come se" si desiderasse produrre dei risultati, e che va a buon fine se e solo se tutti i file di input sono corretti e non vuoti.

##### Forma e contenuto del file di ingresso principale

Il *file di ingresso principale* è un archivio di testo di tipo INI, contenente le seguenti sezioni:

* Sezione [General]
* Sezione [Timing]
* Sezione [Emission]
* Sezione [Meteo]
* Sezione [Output]

Le diverse sezioni contengono le chiavi di configurazione, descritte qui di seguito

##### Sezione [General]

La sezione [General] contiene alcune impostazioni di carattere generale, che comprendono le seguenti chiavi:

* debug_level: Intero tra 0 e 3 che stabilisce il "livello di debug".
* exec_mode: Intero, che contiene il modo di esecuzione
diafile     = diag.dat
frame_interval = 0
frame_path     = 
profile_path   = 


##### Sezione [Timing]

##### Sezione [Emission]

##### Sezione [Meteo]

##### Sezione [Output]



#### Il file meteorologico

Il file meteorologico è un testo in formato CSV, con separatore di campo virgola e punto decimale. Non contiene righe di intestazione.

Le righe dati hanno tutte lo stesso identico formato, caratterizzato dal seguente tracciato:

1. Marca temporale in formato ISO (esempio: “2019-01-28 12:10:00”).

2. Temperatura dell’aria (°C)

3. Velocità del vento (m/s)

4. Direzione di provenienza del vento (° da Nord, contati in verso orario)

5. Velocità di frizione (m/s)

6. Flusso turbolento di calore sensibile (W/m2)

7. Spessore del PBL (m).

Le marche temporali delle righe successive devono essere ordinate in senso crescente, e la differenza tra due marche temporali di righe consecutive deve essere fissa (non è obbligatorio sia oraria), e, naturalmente, conosciuta; non è ammessa la presenza di “buchi” e dati invalidi: in caso di necessità, la formazione del file meteorologico può richiedere l’uso di un’apposita procedura di *gap filling* o un processore meteorologico.

#### Il file delle sorgenti statiche

To apply any text formatting you can see on this page with just a tap, in the Home tab of the ribbon, take a look at Styles.
