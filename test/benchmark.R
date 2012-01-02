# TODO: Add comment
# 
# Author: jeroen
###############################################################################


library(Mobilize);
oh.login("ohmage.admin", "Very.healthy1", "https://lausd.mobilizingcs.org/app");
campaigns <- row.names(oh.campaign.read());
campaigns <- grep("urn:campaign:loadtest:jeroen", campaigns, value=T);
sizes <- as.numeric(substring(campaigns, 30));
myorder <- order(sizes);
campaigns <- campaigns[myorder];
sizes <- sizes[myorder];

testdata <- data.frame(size=sizes, user.self=NA, sys.self=NA, elapsed=NA, user.child=NA, sys.child=NA);
for(i in 1:length(campaigns)){
	cat("Benchmarking size: ", sizes[i], "\n");
	testdata[i, 2:6] <- unclass(system.time(oh.survey_response.read(campaigns[i])));
}


