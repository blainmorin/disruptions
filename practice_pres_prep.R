

library(tidyverse)
library(ggridges)
library(haven)
library(plotly)


wide.df =  read_dta("Combined_Sandy_Harvey-Oct-08-2019-wide.dta")

wide.df = wide.df %>%
  filter(all_outliers == 0)

harvey = wide.df %>%
  filter(hurricane == 1) %>%
  filter(none == 0)

###############################################################################
###############################################################################


#### Observed w RM and wRM Figure

hdf = harvey %>%
  select(vrec, 
         exp_sales_annual_if_no_hurricane, 
         annual_lost_sales__percent, 
         avoided_losses_using_tactics, 
         months_to_recover,
         industry_codes) %>%
  mutate(ySR_loss = exp_sales_annual_if_no_hurricane * annual_lost_sales__percent / 100) %>%
  mutate(max_pot_loss = ySR_loss + avoided_losses_using_tactics) %>%
  mutate(RM = avoided_losses_using_tactics / max_pot_loss) %>%
  mutate(w = max_pot_loss / exp_sales_annual_if_no_hurricane) %>%
  mutate(w = ifelse(w > 1, 1, w)) %>%
  mutate(wRM = w * RM) %>%
  mutate(industry_codes = as.factor(industry_codes))

hdf$industry_codes1 = hdf$industry_codes
levels(hdf$industry_codes1) = c("Agriculture, Mining,\nand Construction",
                               "Transportation, Communications,\nand Utilities",
                               "Manufacturing",
                               "Wholesale, Retail, Trade",
                               "Finance, Insurance,\nand Real Estate",
                               "Service Sector")


temp1 = hdf %>%
  mutate("Lost SR" = annual_lost_sales__percent / 100) %>%
  select(vrec,industry_codes1, w, RM, wRM, "Lost SR")

temp1 = temp1 %>%
  gather(key = "Metric", value = "Value", w:"Lost SR")

temp1$Metric = factor(temp1$Metric, levels = c("w", "RM", "wRM", "Lost SR"))

a = temp1 %>%
  ggplot(aes(x = Metric, y = Value)) +
  geom_boxplot(aes(fill = Metric)) +
  theme_bw() +
  ylab("") +
  xlab("") +
  facet_wrap(~industry_codes1) +
  theme(strip.background =element_rect(fill="black")) +
  theme(strip.text = element_text(colour = 'white')) +
  scale_fill_brewer()


a


###############################################################################
###############################################################################

### BI's by sector


hdf = harvey %>%
  select(vrec, 
         exp_sales_annual_if_no_hurricane, 
         annual_lost_sales__percent, 
         avoided_losses_using_tactics, 
         months_to_recover,
         industry_codes,
         starts_with("bi", ignore.case = FALSE)) %>%
  mutate(ySR_loss = exp_sales_annual_if_no_hurricane * annual_lost_sales__percent / 100) %>%
  mutate(max_pot_loss = ySR_loss + avoided_losses_using_tactics) %>%
  mutate(RM = avoided_losses_using_tactics / max_pot_loss) %>%
  mutate(w = max_pot_loss / exp_sales_annual_if_no_hurricane) %>%
  mutate(w = ifelse(w > 1, 1, w)) %>%
  mutate(wRM = w * RM) %>%
  mutate(industry_codes = as.factor(industry_codes))

hdf$industry_codes1 = hdf$industry_codes
levels(hdf$industry_codes1) = c("Agriculture, Mining,\nand Construction",
                                "Transportation, Communications,\nand Utilities",
                                "Manufacturing",
                                "Wholesale, Retail, Trade",
                                "Finance, Insurance,\nand Real Estate",
                                "Service Sector")


hdff = hdf %>%
  gather(key = "BI", value = "Percent", bi_employees_unable_to_work:bi_facility_underwater)

hdff$BI = factor(hdff$BI)
levels(hdff$BI) = c("Communications", "Employees Moved", "Employees Can't Work", "Facility Underwater",
                    "Gas Outage", "Power Outage", "Supply Chain", "Transportation", "Water Outage")

b = hdff %>%
  filter(Percent != 0) %>%
  ggplot(aes(x = BI, y = Percent)) +
  geom_boxplot(aes(fill = BI)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 65, hjust = 1, vjust = 1)) +
  theme(legend.position = "none") +
  facet_wrap(~industry_codes1) +
  xlab("Disruption Type") +
  theme(strip.background =element_rect(fill="black")) +
  theme(strip.text = element_text(colour = 'white')) +
  scale_fill_brewer()
  
b


###############################################################################
###############################################################################

# Number of Tactics Used


hdf = harvey %>%
  select(vrec, 
         exp_sales_annual_if_no_hurricane, 
         annual_lost_sales__percent, 
         avoided_losses_using_tactics, 
         months_to_recover,
         industry_codes,
         starts_with("bi", ignore.case = FALSE),
         43:53) %>%
  mutate(ySR_loss = exp_sales_annual_if_no_hurricane * annual_lost_sales__percent / 100) %>%
  mutate(max_pot_loss = ySR_loss + avoided_losses_using_tactics) %>%
  mutate(RM = avoided_losses_using_tactics / max_pot_loss) %>%
  mutate(w = max_pot_loss / exp_sales_annual_if_no_hurricane) %>%
  mutate(w = ifelse(w > 1, 1, w)) %>%
  mutate(wRM = w * RM) %>%
  mutate(industry_codes = as.factor(industry_codes)) 

hdf$industry_codes1 = hdf$industry_codes
levels(hdf$industry_codes1) = c("Agriculture, Mining,\nand Construction",
                                "Transportation, Communications,\nand Utilities",
                                "Manufacturing",
                                "Wholesale, Retail, Trade",
                                "Finance, Insurance,\nand Real Estate",
                                "Service Sector")


hdff = hdf %>%
  select(conservation:resource_sharing) %>%
  rowSums()

hdf$n_tactics = hdff

hdfff = hdf %>%
  select(vrec, industry_codes1, n_tactics, w, RM, wRM, annual_lost_sales__percent) %>%
  mutate(annual_lost_sales__percent = annual_lost_sales__percent / 100) %>%
  rename("Lost SR" = annual_lost_sales__percent) %>%
  gather(key = "Metric", value = "Percent", w:"Lost SR")

c = hdfff %>%
  ggplot(aes(x = Percent, y = n_tactics)) +
  geom_point(aes(color = Metric), alpha = .6) +
  geom_smooth(method = "lm", aes(color = Metric), se = FALSE, size = 1.2, alpha = .8) +
  facet_wrap(~industry_codes1) +
  theme_bw() +
  theme(strip.background =element_rect(fill="black")) +
  theme(strip.text = element_text(colour = 'white')) +
  ylab("Number of Tactics Used") +
  xlab("") +
  scale_color_brewer(type = "qual", palette = 3)
  
ggplotly(c)


###############################################################################
###############################################################################

### w and wRM versus BI

hdf = harvey %>%
  select(vrec, 
         exp_sales_annual_if_no_hurricane, 
         annual_lost_sales__percent, 
         avoided_losses_using_tactics, 
         months_to_recover,
         industry_codes,
         starts_with("bi", ignore.case = FALSE),
         43:53) %>%
  mutate(ySR_loss = exp_sales_annual_if_no_hurricane * annual_lost_sales__percent / 100) %>%
  mutate(max_pot_loss = ySR_loss + avoided_losses_using_tactics) %>%
  mutate(RM = avoided_losses_using_tactics / max_pot_loss) %>%
  mutate(w = max_pot_loss / exp_sales_annual_if_no_hurricane) %>%
  mutate(w = ifelse(w > 1, 1, w)) %>%
  mutate(wRM = w * RM) %>%
  mutate(industry_codes = as.factor(industry_codes)) 

hdf$industry_codes1 = hdf$industry_codes
levels(hdf$industry_codes1) = c("Agriculture, Mining,\nand Construction",
                                "Transportation, Communications,\nand Utilities",
                                "Manufacturing",
                                "Wholesale, Retail, Trade",
                                "Finance, Insurance,\nand Real Estate",
                                "Service Sector")


hdff = hdf %>%
  gather(key = "BI_Type", value = "Percent", bi_employees_unable_to_work:bi_other)

hdff$BI_Type = as.factor(hdff$BI_Type)
levels(hdff$BI_Type) = c("Communication",
                         "Employees Moved",
                         "Employees Can't Work",
                         "Facility Underwater",
                         "Gas Outage",
                         "Other",
                         "Power Outage",
                         "Supply Chain",
                         "Transportation",
                         "Water Outage")


d = hdff %>%
  ggplot(aes(x = Percent, y = w)) +
  geom_point() +
  facet_wrap(~BI_Type) +
  theme_bw() +
  theme(strip.background =element_rect(fill="black")) +
  theme(strip.text = element_text(colour = 'white')) 

d

e = hdff %>%
  ggplot(aes(x = Percent, y = wRM)) +
  geom_point() +
  facet_wrap(~BI_Type) +
  theme_bw() +
  theme(strip.background =element_rect(fill="black")) +
  theme(strip.text = element_text(colour = 'white')) 

e

hdff = hdff %>%
  mutate(iw = ifelse(w <= .5, "Low", "High")) %>%
  mutate(iwRM = ifelse(wRM <= .3, "Low", "High"))

f = hdff %>%
  drop_na(iw, iwRM) %>%
  filter(Percent != 0) %>%
  ggplot(aes(x = Percent, y = iw)) +
  geom_density_ridges(aes(fill = iw), scale = 1) +
  facet_wrap(~BI_Type) +
  scale_y_discrete(limits = c("Low", "High")) +
  theme_bw() +
  theme(strip.background =element_rect(fill="black")) +
  theme(strip.text = element_text(colour = 'white')) +
  scale_fill_brewer(type = "qual", palette = 3) +
  ylab("w") +
  xlab("Percent Contribution to Business Interruption") +
  theme(legend.position = "none") 
f

g = hdff %>%
  drop_na(iw, iwRM) %>%
  filter(Percent != 0) %>%
  ggplot(aes(x = Percent, y = iwRM)) +
  geom_density_ridges(aes(fill = iwRM), scale = 1) +
  facet_wrap(~BI_Type) +
  scale_y_discrete(limits = c("Low", "High")) +
  theme_bw() +
  theme(strip.background =element_rect(fill="black")) +
  theme(strip.text = element_text(colour = 'white')) +
  scale_fill_brewer(type = "qual", palette = 3) +
  ylab("wRM") +
  xlab("Percent Contribution to Business Interruption") +
  theme(legend.position = "none") 

g


###############################################################################
###############################################################################


### BI low versus high

hdf = harvey %>%
  select(vrec, 
         exp_sales_annual_if_no_hurricane, 
         annual_lost_sales__percent, 
         avoided_losses_using_tactics, 
         months_to_recover,
         industry_codes,
         starts_with("bi", ignore.case = FALSE),
         43:53) %>%
  mutate(ySR_loss = exp_sales_annual_if_no_hurricane * annual_lost_sales__percent / 100) %>%
  mutate(max_pot_loss = ySR_loss + avoided_losses_using_tactics) %>%
  mutate(RM = avoided_losses_using_tactics / max_pot_loss) %>%
  mutate(w = max_pot_loss / exp_sales_annual_if_no_hurricane) %>%
  mutate(w = ifelse(w > 1, 1, w)) %>%
  mutate(wRM = w * RM) %>%
  mutate(industry_codes = as.factor(industry_codes)) 

hdf$industry_codes1 = hdf$industry_codes
levels(hdf$industry_codes1) = c("Agriculture, Mining,\nand Construction",
                                "Transportation, Communications,\nand Utilities",
                                "Manufacturing",
                                "Wholesale, Retail, Trade",
                                "Finance, Insurance,\nand Real Estate",
                                "Service Sector")


hdff = hdf %>%
  gather(key = "BI_Type", value = "Percent", bi_employees_unable_to_work:bi_facility_underwater)

hdff$BI_Type = as.factor(hdff$BI_Type)
levels(hdff$BI_Type) = c("Communication",
                         "Employees Moved",
                         "Employees Can't Work",
                         "Facility Underwater",
                         "Gas Outage",
                         "Power Outage",
                         "Supply Chain",
                         "Transportation",
                         "Water Outage")

hdfff = hdff %>%
  group_by(BI_Type) %>%
  mutate(cutoff = quantile(Percent, probs = .7, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(iBI = ifelse(Percent > cutoff, "High", "Low"))

h = hdfff %>%
  ggplot(aes(x = w, y = iBI)) +
  geom_density_ridges(aes(fill = iBI), quantiles = 2, quantile_lines = TRUE) +
  facet_wrap(~BI_Type) +
  scale_y_discrete(limits = c("Low", "High")) +
  theme_bw() +
  theme(strip.background =element_rect(fill="black")) +
  theme(strip.text = element_text(colour = 'white')) +
  scale_fill_brewer(type = "qual", palette = 3) +
  ylab("Percent Contributing To BI") +
  xlab("w") +
  theme(legend.position = "none") 

h

i = hdfff %>%
  ggplot(aes(x = wRM, y = iBI)) +
  geom_density_ridges(aes(fill = iBI), quantiles = 2, quantile_lines = TRUE) +
  facet_wrap(~BI_Type) +
  scale_y_discrete(limits = c("Low", "High")) +
  theme_bw() +
  theme(strip.background =element_rect(fill="black")) +
  theme(strip.text = element_text(colour = 'white')) +
  scale_fill_brewer(type = "qual", palette = 3) +
  ylab("Percent Contributing To BI") +
  xlab("wRM") +
  theme(legend.position = "none") 

i  

j = hdfff %>%
  ggplot(aes(x = annual_lost_sales__percent, y = iBI)) +
  geom_density_ridges(aes(fill = iBI), quantiles = 2, quantile_lines = TRUE) +
  facet_wrap(~BI_Type) +
  scale_y_discrete(limits = c("Low", "High")) +
  theme_bw() +
  theme(strip.background =element_rect(fill="black")) +
  theme(strip.text = element_text(colour = 'white')) +
  scale_fill_brewer(type = "qual", palette = 3) +
  ylab("Percent Contributing To BI") +
  xlab("Percent Lost Sales Revenue") +
  theme(legend.position = "none") 

j

###############################################################################
###############################################################################


### Tactics and BI

hdf = harvey %>%
  select(vrec, 
         exp_sales_annual_if_no_hurricane, 
         annual_lost_sales__percent, 
         avoided_losses_using_tactics, 
         months_to_recover,
         industry_codes,
         starts_with("bi", ignore.case = FALSE),
         43:53) %>%
  mutate(ySR_loss = exp_sales_annual_if_no_hurricane * annual_lost_sales__percent / 100) %>%
  mutate(max_pot_loss = ySR_loss + avoided_losses_using_tactics) %>%
  mutate(RM = avoided_losses_using_tactics / max_pot_loss) %>%
  mutate(w = max_pot_loss / exp_sales_annual_if_no_hurricane) %>%
  mutate(w = ifelse(w > 1, 1, w)) %>%
  mutate(wRM = w * RM) %>%
  mutate(industry_codes = as.factor(industry_codes)) 

hdf$industry_codes1 = hdf$industry_codes
levels(hdf$industry_codes1) = c("Agriculture, Mining,\nand Construction",
                                "Transportation, Communications,\nand Utilities",
                                "Manufacturing",
                                "Wholesale, Retail, Trade",
                                "Finance, Insurance,\nand Real Estate",
                                "Service Sector")


hdff = hdf %>%
  gather(key = "BI_Type", value = "Percent", bi_employees_unable_to_work:bi_facility_underwater)

hdff$BI_Type = as.factor(hdff$BI_Type)
levels(hdff$BI_Type) = c("Communications",
                         "Employees Moved",
                         "Employees Can't Work",
                         "Facility Underwater",
                         "Gas Outage",
                         "Power Outage",
                         "Supply Chain",
                         "Transportation",
                         "Water Outage")


hdfff = hdff %>%
  gather(key = "Tactic", "Use", conservation:resource_sharing)

hdfff$Tactic = as.factor(hdfff$Tactic)
levels(hdfff$Tactic) = c("Conservation",
                         "Excess Capacity",
                         "Import Substitution",
                         "Input Substitution",
                         "Inventories",
                         "Management\nEffectiveness",
                         "Production\nRecapture",
                         "Relocation",
                         "Resource\nIsolation",
                         "Resource\nSharing",
                         "Technological\nChange")


k = hdfff %>%
  filter(Use == 1) %>%
  filter(Percent > 0) %>%
  ggplot(aes(y = BI_Type, x = Percent)) +
  geom_density_ridges(aes(fill = BI_Type), rel_min_height = .02) +
  facet_wrap(~Tactic) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(strip.background =element_rect(fill="black")) +
  theme(strip.text = element_text(colour = 'white')) +
  scale_fill_brewer() +
  xlim(0,100)

k

###############################################################################
###############################################################################


### Tactics and w SR

hdf = harvey %>%
  select(vrec, 
         exp_sales_annual_if_no_hurricane, 
         annual_lost_sales__percent, 
         avoided_losses_using_tactics, 
         months_to_recover,
         industry_codes,
         starts_with("bi", ignore.case = FALSE),
         43:53) %>%
  mutate(ySR_loss = exp_sales_annual_if_no_hurricane * annual_lost_sales__percent / 100) %>%
  mutate(max_pot_loss = ySR_loss + avoided_losses_using_tactics) %>%
  mutate(RM = avoided_losses_using_tactics / max_pot_loss) %>%
  mutate(w = max_pot_loss / exp_sales_annual_if_no_hurricane) %>%
  mutate(w = ifelse(w > 1, 1, w)) %>%
  mutate(wRM = w * RM) %>%
  mutate(industry_codes = as.factor(industry_codes)) 

hdf$industry_codes1 = hdf$industry_codes
levels(hdf$industry_codes1) = c("Agriculture, Mining,\nand Construction",
                                "Transportation, Communications,\nand Utilities",
                                "Manufacturing",
                                "Wholesale, Retail, Trade",
                                "Finance, Insurance,\nand Real Estate",
                                "Service Sector")


hdff = hdf %>%
  gather(key = "BI_Type", value = "Percent", bi_employees_unable_to_work:bi_facility_underwater)

hdff$BI_Type = as.factor(hdff$BI_Type)
levels(hdff$BI_Type) = c("Communications",
                         "Employees Moved",
                         "Employees Can't Work",
                         "Facility Underwater",
                         "Gas Outage",
                         "Power Outage",
                         "Supply Chain",
                         "Transportation",
                         "Water Outage")


hdfff = hdff %>%
  gather(key = "Tactic", "Use", conservation:resource_sharing)

hdfff$Tactic = as.factor(hdfff$Tactic)
levels(hdfff$Tactic) = c("Conservation",
                         "Excess Capacity",
                         "Import Substitution",
                         "Input Substitution",
                         "Inventories",
                         "Management\nEffectiveness",
                         "Production\nRecapture",
                         "Relocation",
                         "Resource\nIsolation",
                         "Resource\nSharing",
                         "Technological\nChange")


hd4 = hdfff %>%
  filter(Use == 1) %>%
  mutate("Lost SR" = annual_lost_sales__percent / 100) %>%
  gather(key = "Metric", value = "Value", c("Lost SR", w)) 

l = hd4 %>%
  ggplot(aes(x = Metric, y = Value)) +
  geom_boxplot(aes(fill = Metric)) +
  facet_wrap(~Tactic)+
  theme_bw() +
  theme(strip.background =element_rect(fill="black")) +
  theme(strip.text = element_text(colour = 'white')) +
  scale_fill_brewer() +
  xlab("")

l
