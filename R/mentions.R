all.mentions %>%
  group_by(publication, organization) %>%
  summarize(thing=sum(num.mentions))

all.mentions %>%
  group_by(organization) %>%
  summarize(thing=sum(num.mentions))

all.mentions %>%
  group_by(publication) %>%
  summarize(thing=sum(num.mentions))
