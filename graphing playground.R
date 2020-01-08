ggplot() + 
  geom_line(plotting, aes(Year,v2x_polyarchy, group = RecipientName), color = 'lightgrey') +
  geom_line(filter(plotting,majority == "Donor Gov"),aes(Year,v2x_polyarchy, group = RecipientName), color = 'red')


plotting %>%
  filter(Year==2014) %>%
  group_by(RecipientName) %>%
  summarise(total = log(sum(total_disbursement, na.rm = TRUE)+1), polity2 = mean(polity2,na.rm = TRUE, total_diff = diff(total))) %>%
  ggplot(aes(polity2,total_diff)) + 
    geom_text(aes(label = RecipientName))


ts <- ts(plotting)
ts


ggplot() +
  geom_line(data = plotting[plotting$majority == 'Donor Gov',], aes(x=Year,y=v2x_polyarchy, group = RecipientName), color = 'blue') + 
  geom_line(data = plotting[plotting$majority == 'Recipient Gov',], aes(x=Year, y = v2x_polyarchy, group = RecipientName), color = 'red')



working_rec <- working %>%
  group_by(Year,RecipientName) %>%
  summarise(v2x_libdem = mean(v2x_libdem, na.rm = TRUE),v2x_partipdem = mean(v2x_partipdem, na.rm = TRUE),e_fh_status = mean(e_fh_status, na.rm = TRUE),v2x_polyarchy = mean(v2x_polyarchy, na.rm = TRUE),polity = mean(polity2, na.rm = TRUE), total = sum(total_disbursement, na.rm = TRUE), govd = sum(gov_d_total, na.rm = TRUE), govr = sum(gov_r_total, na.rm = TRUE), gov3 = sum(gov_3_total, na.rm = TRUE), govo = sum(gov_o_total, na.rm = TRUE), corp = sum(corp_total, na.rm = TRUE), ngo = sum(ngo_total, na.rm = TRUE), igo = sum(igo_total, na.rm = TRUE), other = sum(other_total, na.rm = TRUE)) %>%
  mutate(govdper = govd/total,govrper = govr/total,gov3per = gov3/total,govoper = govo/total,corpper=corp/total,ngoper=ngo/total,igoper=igo/total,otherper=other/total) %>%
  arrange(RecipientName,Year)

ggplot(working_rec,aes(as.factor(RecipientName))) +
  geom_bar(aes(weight = sum(govd,govr,govo,gov3,corp,ngo,igo,other)))


ggplot(working_rec, aes(Year, polity, group = RecipientName)) +
  geom_line(aes(group = govd/total))



ggplot(working_rec[working_rec$RecipientName=='Pakistan',]) +
  geom_line(aes(Year,ngoper), color= 'red') +
  geom_line(aes(Year,v2x_partipdem), color = 'blue') +
  geom_line(aes(Year,v2x_libdem), color = 'purple') +
  geom_line(aes(Year,v2x_polyarchy), color = 'black')


ggplot(working_rec) +
  geom_line(aes(Year,log(ngo+1)),color='green') + 
  geom_line(aes(Year,log(govd+1)),color='red') + 
  geom_line(aes(Year,log(govr+1)),color='blue') +
  geom_line(aes(Year,v2x_polyarchy*10)) +
  facet_wrap(~RecipientName)
