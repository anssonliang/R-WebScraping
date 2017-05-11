### LOAD PACKAGE                     ###
packages <- c("rvest", "stringr")
invisible(lapply(packages, require, character.only = TRUE))

### TELIA            ###
url = "https://www.telia.dk/privat/abonnementer/mobiltelefoni/"

telia_text <- url %>%
  read_html %>% 
  html_nodes(".a-fee , .right , h4+ p , h4")%>% # CSS selector
  html_text(trim = TRUE)

telia_opt_text <- url %>%
  read_html %>% 
  html_nodes("img")%>% # CSS selector
  html_attr("alt")

telia_tmp <- str_replace_all(telia_text, "\r\n\t\r\n", " ")%>%
  str_replace(" GB", "")%>%
  str_replace(" kr./md", "")%>%
  (function(x)gsub("^.*?1. md. ","",x))%>%
  str_replace(" kr.", "")%>%
  str_replace("^Fri.*","YES")%>%
  str_replace(",",".")

telia_opt_tmp <- telia_opt_text[duplicated(telia_opt_text)]%>%
  (function(x) unique(x[!is.na(x)]))%>%
  paste(collapse=", ")%>%
  rep(4)

telia <- t(telia_tmp[1:4])
for(i in 1:(sum(str_count(telia_tmp, "YES"))-1)){
  telia <- rbind(telia, t(telia_tmp[(1+(i)*4):(4+i*4)]))
}
  
telia        <- as.data.frame(cbind(matrix('telia',nrow(telia)),telia,telia_opt_tmp))
names(telia) <- c("subscription","data.GB","free.voice.sms.mms","price.kr./md","min.price.kr./md","option")
telia$subscription <- paste0(telia$subscription,'_',telia$data.GB,'GB')

### TELENOR       ###
url = "https://www.telenor.dk/shop/abonnementer/"

telenor_text <- url %>%
  read_html %>% 
  html_nodes(".padding-toright--small .text-size--11:nth-child(1) , #plan_1 li")%>% # CSS selector
  html_text()

telenor_opt_text <- url %>%
  read_html %>% 
  html_nodes(".options-list")%>% # CSS selector
  html_text()

telenor_tmp <- str_replace_all(telenor_text, "\t", "")%>%
               str_replace_all( "\r\n", "")%>%
               (function(x){x[- grep("Alt om|Roam Away|Viaplay",x)]})%>%
               (function(x){x[1:(3*3)]})

telenor_opt_tmp <- gsub("^.*?mms","mms",telenor_opt_text)%>%
  str_replace_all("\t", "")%>%
  str_replace_all( "\r\n", "")%>%
  str_replace( "mms", "")%>%
  (function(x) x[1:3])

price_sp <- as.vector(do.call('rbind', str_split(telenor_tmp[7:9]," · ")))

telenor_tmp <- c(telenor_tmp[1:6],as.vector(do.call('rbind', str_split(telenor_tmp[7:9]," · "))))%>%
               str_replace(" GB data", "")%>%
               str_replace("^Fri.*","YES")%>%
               str_replace(",- /md.","")%>%
               str_replace("Mindstepris ","")%>%
               str_replace(",-","")

telenor <- rbind(t(telenor_tmp[c(1,2,7,10)]),t(telenor_tmp[c(3,4,8,11)]),t(telenor_tmp[c(5,6,9,12)]))
telenor <- as.data.frame(cbind(matrix('telenor',nrow(telenor)),telenor,telenor_opt_tmp))
names(telenor) <- c("subscription","data.GB","free.voice.sms.mms","price.kr./md","min.price.kr./md","option")
telenor$subscription <- paste0(telenor$subscription,'_',telenor$data.GB,'GB')

### TDC       ###
url = "https://yousee.dk/mobil/"

tdc_text <- url %>%
  read_html %>% 
  html_nodes(".show-min-price-box , .lg-price .price , .subheader , .data-num")%>% # CSS selector
  html_text()

tdc_opt_text <- url %>%
  read_html %>% 
  html_nodes(".inner")%>% # CSS selector
  html_text()

tdc_tmp <- str_replace_all(tdc_text, "\n", "")%>%
  (function(x)gsub("^.*?md.","md.",x))%>%  
  str_replace("md. ","")%>%
  str_replace("kr.","")%>%
  str_trim()%>%
  str_replace("^Fri.*","YES")

tdc_tmp_opt <- gsub("^.*?\n\n","\n\n",tdc_opt_text)%>% 
  str_replace("\n\n","")%>%
  str_replace_all("\n",",")%>%
  str_trim()
  
tdc <- t(tdc_tmp[1:4])
for(i in 1:(sum(str_count(tdc_tmp, "YES"))-1)){
  tdc <- rbind(tdc, t(tdc_tmp[(1+(i)*4):(4+i*4)]))
}

tdc       <- as.data.frame(cbind(matrix('tdc',nrow(tdc)),tdc,tdc_tmp_opt))
names(tdc) <- c("subscription","data.GB","free.voice.sms.mms","price.kr./md","min.price.kr./md","option")
tdc$subscription <- paste0(tdc$subscription,'_',tdc$data.GB,'GB')

### Telmore       ###
url = "https://www.telmore.dk/mobil#Abonnement"

telmore_text <- url %>%
  read_html %>% 
  html_nodes(".bv2_middle , .clear-block .price , .text-center-lg .bv2_top_left span , .clear-block .bv2_top_left span , .bv2_top_right span , .text-center-lg .price")%>% # CSS selector
  html_text()

telmore_tmp <- str_replace_all(telmore_text, "\n", "")%>%
  str_replace("GB data","")%>%
  str_trim()

telmore_tmp_opt <- matrix(rep("",each = 6), nrow=6)

telmore <- t(telmore_tmp[1:6])
for(i in 1:(length(telmore_tmp)/6-1)){
  telmore <- rbind(telmore, t(telmore_tmp[(1+(i)*6):(6+i*6)]))
}

telmore <- cbind(paste0(telmore[,3]," ",telmore[,4]),paste0(telmore[,1]," ",telmore[,2]),telmore[,c(-1,-2,-3,-4)])

telmore <- as.data.frame(cbind(matrix('telmore',nrow(telmore)),telmore[,c(1,2,4)],telmore_tmp_opt,telmore[,3]))

names(telmore) <- c("subscription","data.GB","free.voice.sms.mms","price.kr./md","min.price.kr./md","option")
telmore$subscription <- paste0(telmore$subscription,'_',telmore$data.GB,'GB')

### Telmore       ###
url = "https://www.3.dk/abonnementer/mobil/"

three_text <- url %>%
  read_html %>% 
  html_nodes("#fritale .laursen-abo-sub-container")%>% # CSS selector
  html_text()

three_tmp <- unlist(str_split(three_text, "\n\n\n"))%>%
  (function(x) {x[-1]})%>%
  str_replace_all("\n", "")%>%
  str_replace_all(" kr./md.", "")%>%
  str_replace_all("GB", "")%>%
  (function(x)gsub("^.*?1.md.: ","",x))%>%
  str_replace("^Fri.*","YES")%>%
  str_replace("kr.","")

three <- t(three_tmp[1:5])
for(i in 1:(sum(str_count(three_tmp, "YES"))-1)){
  three <- rbind(three, t(three_tmp[(1+(i)*5):(5+i*5)]))
}

three <- as.data.frame(cbind(matrix('three',nrow(three)),three[,c(2,1,3,4,5)]))
names(three) <- c("subscription","data.GB","free.voice.sms.mms","price.kr./md","min.price.kr./md","option")
three$subscription <- paste0(three$subscription,'_',three$data.GB,'GB')

## COMBINE 4 subscriptions     ##
all <- data.frame()
all <- rbind(telia,telenor,three,tdc, telmore)
all <- cbind(all,"eu_roam"=grepl(paste(c("ROAM","EU"),collapse = "|"),str_to_upper(all$option))
             ,"tv"=grepl("TV",str_to_upper(all$option))
             ,"flipp"=grepl("FLIPP",str_to_upper(all$option))
             ,"zetland"=grepl("ZETLAND",str_to_upper(all$option))
             ,"spotify"=grepl("SPOTIFY",str_to_upper(all$option))
             ,"hbo_nordic"=grepl("HBO",str_to_upper(all$option))
             ,"viaplay"=grepl("VIAPLAY",str_to_upper(all$option))
             ,"yousee_music"=grepl("YOUSEE",str_to_upper(all$option))
             ,"voicemail"=grepl("VOICEMAIL",str_to_upper(all$option))
             ,"datadeling_sim"=grepl("DATADELING",str_to_upper(all$option))
)%>%
  (function(x) {x[,!names(x)=="option"]})
all[all=='TRUE']<-'YES'
all[all=='FALSE']<-'NO'

write.csv(all,"all_subscription.csv")
