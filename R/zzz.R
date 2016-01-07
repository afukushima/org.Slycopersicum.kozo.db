datacache <- new.env(hash=TRUE, parent=emptyenv())

org.Slycopersicum.kozo <- function() showQCData("org.Slycopersicum.kozo", datacache)
org.Slycopersicum.kozo_dbconn <- function() dbconn(datacache)
org.Slycopersicum.kozo_dbfile <- function() dbfile(datacache)
org.Slycopersicum.kozo_dbschema <- function(file="", show.indices=FALSE) dbschema(datacache, file=file, show.indices=show.indices)
org.Slycopersicum.kozo_dbInfo <- function() dbInfo(datacache)

org.Slycopersicum.kozoORGANISM <- "Solanum lycopersicum"

.onLoad <- function(libname, pkgname)
{
    ## Connect to the SQLite DB
    dbfile <- system.file("extdata", "org.Slycopersicum.kozo.sqlite", package=pkgname, lib.loc=libname)
    assign("dbfile", dbfile, envir=datacache)
    dbconn <- dbFileConnect(dbfile)
    assign("dbconn", dbconn, envir=datacache)

    ## Create the OrgDb object
    sPkgname <- sub(".db$","",pkgname)
    db <- loadDb(system.file("extdata", paste(sPkgname,
      ".sqlite",sep=""), package=pkgname, lib.loc=libname),
                   packageName=pkgname)    
    dbNewname <- AnnotationDbi:::dbObjectName(pkgname,"OrgDb")
    ns <- asNamespace(pkgname)
    assign(dbNewname, db, envir=ns)
    namespaceExport(ns, dbNewname)
    
    ## Create the AnnObj instances
#    ann_objs <- createAnnObjs.SchemaChoice("ARABIDOPSIS_DB",
#                                           "org.At.tair", "Arabidopsis", dbconn, datacache)
    SOLANUM_DB_L2Rlink1 <- list(tablename="genes", Lcolname="gene_id", Rcolname="_id")
    SOLANUM_DB_AnnDbBimap_seeds <- list(
      list(
        objName="CHR",
        Class="AnnDbBimap",
        L2Rchain=list(
          SOLANUM_DB_L2Rlink1,
          list(
            tablename="gene_info",
            Lcolname="_id",
            Rcolname="chromosome"
          )
        )
      )
    )
    
    seed0 <- list(
      objTarget="Solanum",
      datacache=datacache
    )
    ann_objs <- AnnotationDbi:::createAnnDbBimaps(SOLANUM_DB_AnnDbBimap_seeds, seed0)
      
    mergeToNamespaceAndExport(ann_objs, pkgname)
    packageStartupMessage(AnnotationDbi:::annoStartupMessages("org.Slycopersicum.kozo.db"))
}

.onUnload <- function(libpath)
{
    dbFileDisconnect(org.Slycopersicum.kozo_dbconn())
}

