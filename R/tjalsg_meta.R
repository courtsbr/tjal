#' Função tjalsg_meta
#'
#' Esta função extrai metadados das decisões de segundo grau do TJAL
#' @param livre palavra ou texto a ser buscado nas ementas e nos acórdãos
#' @param quote logical. Colocar a expressão entre aspas?
#' @param classes.value Código
#' @param inicio  Data inicial
#' @param fim  Data final
#' @keywords tjal, judicial decisions, high court, alagoas
#' @import xml2
#' @import httr
#' @import stringr
#' @export
#' @examples
#' tjalsg_meta(BuscaLivre="Lei Maria da Penha")

tjalsg_meta<-function(BuscaLivre,quote=TRUE,classes.value="",inicio="",fim=""){
  set_config(config(ssl_verifypeer = FALSE ))
  body <- list(dados.buscaInteiroTeor ="", dados.pesquisarComSinonimos = "S",
               dados.pesquisarComSinonimos = "S", dados.buscaEmenta = "",
               dados.nuProcOrigem = "", dados.nuRegistro = "", agenteSelectedEntitiesList = "",
               contadoragente = "0", contadorMaioragente = "0", codigoCr = "",
               codigoTr = "", nmAgente = "", juizProlatorSelectedEntitiesList = "",
               contadorjuizProlator = "0", contadorMaiorjuizProlator = "0",
               codigoJuizCr = "", codigoJuizTr = "", nmJuiz = "", classesTreeSelection.values = "",
               classesTreeSelection.text = "", assuntosTreeSelection.values = "",
               assuntosTreeSelection.text = "", comarcaSelectedEntitiesList = "",
               contadorcomarca = "0", contadorMaiorcomarca = "0", cdComarca = "",
               nmComarca = "", secoesTreeSelection.values = "",
               secoesTreeSelection.text = "1",
               dados.dtJulgamentoInicio = "", dados.dtJulgamentoFim = "",
               dados.dtRegistroInicio = "", dados.dtRegistroFim = "",
               dados.origensSelecionadas = "T", tipoDecisaoSelecionados = "A",
               #tipoDecisaoSelecionados = "", tipoDecisaoSelecionados = "",
               dados.ordenacao = "data")
  if(quote==TRUE) BuscaLivre<-deparse(BuscaLivre)
  body[[1]]<-BuscaLivre
  body[[19]]<-classes.value ##
  body[[30]]<-inicio ## colocar a data no formato dd/mm/aaa
  body[[31]]<-fim # idem
  a<-POST("http://www2.tjal.jus.br/cjsg/resultadoCompleta.do",encode="form",
          body=body)
  b<- content(a,as="parsed")
  max_pag<-b %>% xml_find_all(xpath="//*[@id='totalResultadoAba-A']") %>%
    xml_attr(attr="value") %>%
    as.numeric() %>%
    `/`(15) %>%
    ceiling()

  df<-data.frame()
  for (i in 1:max_pag){
    tryCatch({
      c <- GET(paste0("http://www2.tjal.jus.br/cjsg/trocaDePagina.do?tipoDeDecisao=A&pagina=",i), set_cookies(unlist(a$cookies)))
      d <- content(c,as="parsed")
      aC <-d %>%
        xml_find_all('//*[@class="assuntoClasse"]') %>%
        xml_text(trim=T)
      assunto.processo<-str_trim(str_extract(aC,".*?(?=/)"))
      assunto.materia<-str_trim(str_extract(aC,"(?<=/\\s{0,100}).*"))
      meta<-xml_find_all(d,xpath="//*[@valign='top'][2]") %>% xml_text(trim=T)
      meta<-str_replace_all(meta,"\\s{2,}"," ")
      relator<-str_extract(meta,"Relator.*?(?=\\s(Comarca|Data|Ementa))")
      comarca<-str_extract(meta,"Comarca.*?(?=\\s(Data|Ementa))")
      orgao.julgador<-str_extract(meta,"Órgão.*?(?=\\s(Data|Ementa))")
      data.julgamento<-str_extract(meta,"Data\\sdo.*?(?=\\s(Data|Ementa))")
      data.registro<-str_extract(meta,"Data\\sde.*?(?=\\s(Data|Ementa))")
      ementa<-str_extract(meta,"Ementa.*")
      processo<-str_extract(meta,"\\d+.*?(?=\\s)")
      cdacordao <- xml_find_all(d, xpath = "//*[@class='downloadEmenta']/@cdacordao") %>% 
        xml_text(trim = T)
      df1<-data.frame(pagina=i,assunto.processo,assunto.materia,relator,comarca,orgao.julgador,data.julgamento,data.registro,processo,ementa,cdacordao,stringsAsFactors = F)
      df<-rbind(df,df1)
    },
    error=function(m){
      m
    },
    finally={
      next
    })
    sys.sleep(2)
  }
  df[4:8]<-lapply(df[4:8],function(x) str_replace(x,".*:\\s*",""))
  df$url<-paste0("http://www2.tjal.jus.br/cjsg/getArquivo.do?cdAcordao=",df$cdacordao,"&cdForo=0")

  return(df)
}

