

load("/Users/milogordon/Documents/wildfire_disaster_data/data/processed/combining/04_combine_with_spatial_data/04_make_buffers_and_point_files_from_ics_and_newspaper_data/ics_209_redbook_fema_disasters_no_news_poo_news_acres.rdata")


# at this point we've found all fires with
# ics poo and ics acres
# ics poo and news acres
# news poo and ics acres
# news poo and news acres

# now we will check for ics poo only
# and the last one will be news poo only

ics_209_redbook_fema_disasters_ics_poo_only<-
  ics_209_redbook_fema_disasters_no_news_poo_news_acres%>%
  filter(
    !is.na(ics_209_poo_latitude))%>%
  filter(implausible_poo==0)

ics_209_redbook_fema_disasters_no_ics_poo_only<-
  ics_209_redbook_fema_disasters_no_news_poo_news_acres%>%
  filter(!disaster_nested_id%in%ics_209_redbook_fema_disasters_ics_poo_only$disaster_nested_id)

rm(ics_209_redbook_fema_disasters_no_news_poo_news_acres)

# perimeter source variable -----------------------------------------------

ics_209_redbook_fema_disasters_ics_poo_only<-
  ics_209_redbook_fema_disasters_ics_poo_only%>%
  mutate(
    perimeter_source="ICS POO POINTS"
  )

# save --------------------------------------------------------------------

# ics poo and news acres
save(ics_209_redbook_fema_disasters_ics_poo_only,
     file="/Users/milogordon/Documents/wildfire_disaster_data/data/processed/combining/04_combine_with_spatial_data/04_make_buffers_and_point_files_from_ics_and_newspaper_data/ics_209_redbook_fema_disasters_ics_poo_only.rdata"
)

# ics poo and news acres
save(ics_209_redbook_fema_disasters_no_ics_poo_only,
     file="/Users/milogordon/Documents/wildfire_disaster_data/data/processed/combining/04_combine_with_spatial_data/04_make_buffers_and_point_files_from_ics_and_newspaper_data/ics_209_redbook_fema_disasters_no_ics_poo_only.rdata"
)




