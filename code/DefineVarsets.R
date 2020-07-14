# As in the original paper. This file creates all the variable that group
# individual outcomes into families.

vset_list <- c(
  "sub_time",
  "sub_news",
  "polarize",
  "voting",
  "news",
  "swb",
  "social",
  "fbopinions",
  "postexp",
  "secondary"
)

## List of main outcomes within outcome families

varset_sub_time <- c(
  "fb_min_wins",
  "leisure_sm",
  "leisure_oo",
  "leisure_tvm",
  "leisure_nns",
  "leisure_frf"
)
varset_sub_news <- c(
  "news_source_6",
  "tweets",
  "news_source_7",
  "news_source_8",
  "news_source_3",
  "news_source_4",
  "news_source_5",
  "news_source_1",
  "news_source_2"
)
varset_news <- c(
  "follow_politics",
  "follow_trump",
  "read_news_wins",
  "news_knowledge_score",
  "fake_news_score"
)
varset_voting <- c(
  "vote_2018",
  "clicked_politics_email"
)
varset_polarize <- c(
  "party_thermo",
  "trump_thermo",
  "party_anger",
  "party_understand",
  "issue_polar",
  "belief_polar",
  "vote_polar"
)
varset_swb <- c(
  "swb_happy",
  "swb_swl",
  "swb_lns",
  "swb_eurhappsvy_4",
  "swb_eurhappsvy_5",
  "swb_eurhappsvy_6",
  "swb_eurhappsvy_7",
  "happy_sms_summary",
  "pos_emotion_sms_summary",
  "lonely_sms_summary"
)
varset_social <- c(
  "friends_met_number",
  "leisure_offline", "leisure_diverse"
)
varset_fbopinions <- c(
  "fb_soclife",
  "fb_goodbad",
  "fb_society",
  "fb_happy",
  "fb_polarize",
  "fb_betterfollownews",
  "fb_morefakenews",
  "fb_habit",
  "fb_deact_good",
  "fb_posimp_log",
  "fb_negimp_log"
)
varset_postexp <- c(
  "planned_use",
  "clicked_timelimit_email",
  "time_to_reactivation",
  "mobile_minutes_wins"
)
varset_secondary <- c(
  "vote_conv_resc",
  "vote_yesno"
)

## List of variables in indices
for (vset in vset_list) {
  assign(paste("varset_", vset, "_idx", sep = ""), c())
  assign(paste("varset_", vset, "_idx_nn", sep = ""), c())
  for (var in eval(parse(text = paste("varset_", vset, sep = "")))) {
    if (var != "fb_min_wins" & var != "news_source_6") {
      assign(
        paste("varset_", vset, "_idx", sep = ""),
        c(eval(parse(
          text = paste("varset_", vset, "_idx", sep = "")
        )), var)
      )
      assign(
        paste("varset_", vset, "_idx_nn", sep = ""),
        c(eval(parse(
          text = paste("varset_", vset, "_idx_nn", sep = "")
        )), paste(var, "_nn", sep = ""))
      )
    }
  }
}

## List of variables in polarization Index for polarization robustness check.
extravars <- c()
for (var_excl in varset_polarize) {
  assign(paste("varset_", var_excl, "_idx", sep = ""), c())
  for (pol_var in varset_polarize) {
    if (pol_var != var_excl) {
      assign(
        paste("varset_", var_excl, "_idx", sep = ""),
        c(eval(parse(
          text = paste("varset_", var_excl, "_idx", sep = "")
        )), pol_var)
      )
    }
  }
  extravars <- c(extravars, var_excl)
}

## Labels of index variables
indexname_sub_time <- "Substitute time uses index"
indexname_sub_news <- "Substitute news sources index"
indexname_news <- "News knowledge index"
indexname_voting <- "Political engagement index"
indexname_polarize <- "Political polarization index"
indexname_swb <- "Subjective well-being index"
indexname_social <- "Social interaction index"
indexname_fbopinions <- "Facebook opinions index"
indexname_postexp <- "Post-experiment use index"

## Labels of variables
name_fb_min_wins <- "Facebook minutes"
name_leisure_sm <- "Non-FB social media time"
name_leisure_oo <- "Non-social online time"
name_leisure_tvm <- "TV alone time"
name_leisure_nns <- "Non-screen alone time"
name_leisure_frf <- "Friends and family time"
name_index_sub_time <- "Substitute time uses index"

name_news_source_6 <- "Facebook news"
name_tweets <- "Number of tweets"
name_news_source_7 <- "Non-FB social media news"
name_news_source_8 <- "Non-social online news"
name_news_source_3 <- "Local TV news"
name_news_source_4 <- "Network TV news"
name_news_source_5 <- "Cable TV news"
name_news_source_1 <- "Print news"
name_news_source_2 <- "Radio news"
name_index_sub_news <- "Substitute news sources index"

name_party_thermo <- "Party affective polarization"
name_trump_thermo <- "Trump affective polarization"
name_party_anger <- "Party anger"
name_party_understand <- "Congenial news exposure"
name_issue_polar <- "Issue polarization"
name_belief_polar <- "Belief polarization"
name_vote_polar <- "Vote polarization"
name_index_polarize <- "Political polarization index"

name_vote_2018 <- "Voted"
name_clicked_politics_email <- "Clicked politics email"
name_index_voting <- "Political engagement index"

name_follow_politics <- "Follow politics"
name_follow_trump <- "Follow Trump"
name_read_news_wins <- "News minutes"
name_news_knowledge_score <- "News knowledge"
name_fake_news_score <- "Fake news knowledge"
name_index_news <- "News knowledge index"

name_swb_happy <- "Happiness"
name_swb_swl <- "Life satisfaction"
name_swb_lns <- "Loneliness × (−1)"
name_swb_eurhappsvy_4 <- "Depressed × (−1)"
name_swb_eurhappsvy_5 <- "Anxious × (−1)"
name_swb_eurhappsvy_6 <- "Absorbed"
name_swb_eurhappsvy_7 <- "Bored × (−1)"
name_happy_sms_summary <- "SMS happiness"
name_pos_emotion_sms_summary <- "SMS positive emotion"
name_lonely_sms_summary <- "SMS not lonely"
name_index_swb <- "Subjective well-being index"

name_friends_met_number <- "Friends met in person"
name_leisure_offline <- "Offline activities"
name_leisure_diverse <- "Diverse interactions"
name_index_social <- "Social interaction index"

name_fb_soclife <- "Improves social life"
name_fb_goodbad <- "Good for you"
name_fb_society <- "Good for society"
name_fb_happy <- "Makes people happy"
name_fb_polarize <- "Less polarized"
name_fb_betterfollownews <- "Helps follow news"
name_fb_morefakenews <- "Clickbait, fake news × (−1)"
name_fb_habit <- "People would miss Facebook"
name_fb_deact_good <- "Deactivation bad"
name_fb_posimp_log <- "Positive impacts"
name_fb_negimp_log <- "Negative impacts × (−1)"
name_index_fbopinions <- "Facebook opinions index"

name_planned_use <- "Planned post-study use change"
name_clicked_timelimit_email <- "Clicked time limit email × (−1)"
name_time_to_reactivation <- "Speed of reactivation"
name_mobile_minutes_wins <- "Facebook mobile app use"
name_index_postexp <- "Post-experiment use index"

name_happy_sms<- "SMS happiness"
name_pos_emotion_sms<- "SMS positive emotion"
name_lonely_sms<- "SMS not lonely"

## List of Heterogeneous Treatment Effect (HTE) outcome vars
for (mod in c(
  "W0",
  "W1",
  "A0",
  "A1",
  "M0",
  "M1",
  "N0",
  "N1",
  "P2",
  "P3",
  "F0",
  "F1",
  "R0",
  "R1",
  "O0",
  "O1",
  "K0",
  "K1"
)) {
  assign(
    paste("vs_sub_time_", mod, sep = ""),
    c(
      paste("fb_min_wins_", mod, sep = ""),
      paste("leisure_sm_", mod, sep = ""),
      paste("leisure_oo_", mod, sep = ""),
      paste("leisure_tvm_", mod, sep = ""),
      paste("leisure_nns_", mod, sep = ""),
      paste("leisure_frf_", mod, sep = ""),
      paste("index_sub_time_", mod, sep = "")
    )
  )
  assign(
    paste("vs_sub_news_", mod, sep = ""),
    c(
      paste("news_source_6_", mod, sep = ""),
      paste("tweets_", mod, sep = ""),
      paste("news_source_7_", mod, sep = ""),
      paste("news_source_8_", mod, sep = ""),
      paste("news_source_3_", mod, sep = ""),
      paste("news_source_4_", mod, sep = ""),
      paste("news_source_5_", mod, sep = ""),
      paste("news_source_1_", mod, sep = ""),
      paste("news_source_2_", mod, sep = ""),
      paste("index_sub_news_", mod, sep = "")
    )
  )
  assign(
    paste("vs_news_", mod, sep = ""),
    c(
      paste("follow_politics_", mod, sep = ""),
      paste("follow_trump_", mod, sep = ""),
      paste("read_news_wins_", mod, sep = ""),
      paste("news_knowledge_score_", mod, sep = ""),
      paste("fake_news_score_", mod, sep = ""),
      paste("index_news_", mod, sep = "")
    )
  )
  assign(
    paste("vs_voting_", mod, sep = ""),
    c(
      paste("vote_2018_", mod, sep = ""),
      paste("clicked_politics_email_", mod, sep = ""),
      paste("index_voting_", mod, sep = "")
    )
  )
  assign(
    paste("vs_polarize_", mod, sep = ""),
    c(
      paste("party_thermo_", mod, sep = ""),
      paste("trump_thermo_", mod, sep = ""),
      paste("party_anger_", mod, sep = ""),
      paste("party_understand_", mod, sep = ""),
      paste("issue_polar_", mod, sep = ""),
      paste("belief_polar_", mod, sep = ""),
      paste("vote_polar_", mod, sep = ""),
      paste("index_polarize_", mod, sep = "")
    )
  )
  assign(
    paste("vs_swb_", mod, sep = ""),
    c(
      paste("swb_happy_", mod, sep = ""),
      paste("swb_swl_", mod, sep = ""),
      paste("swb_lns_", mod, sep = ""),
      paste("swb_eurhappsvy_4_", mod, sep = ""),
      paste("swb_eurhappsvy_5_", mod, sep = ""),
      paste("swb_eurhappsvy_6_", mod, sep = ""),
      paste("swb_eurhappsvy_7_", mod, sep = ""),
      paste("happy_sms_summary_", mod, sep = ""),
      paste("pos_emotion_sms_summary_", mod, sep = ""),
      paste("lonely_sms_summary_", mod, sep = ""),
      paste("index_swb_", mod, sep = "")
    )
  )
  assign(
    paste("vs_social_", mod, sep = ""),
    c(
      paste("friends_met_number_", mod, sep = ""),
      paste("leisure_offline_", mod, sep = ""),
      paste("leisure_diverse_", mod, sep = ""),
      paste("index_social_", mod, sep = "")
    )
  )
  assign(
    paste("vs_fbopinions_", mod, sep = ""),
    c(
      paste("fb_soclife_", mod, sep = ""),
      paste("fb_goodbad_", mod, sep = ""),
      paste("fb_society_", mod, sep = ""),
      paste("fb_happy_", mod, sep = ""),
      paste("fb_polarize_", mod, sep = ""),
      paste("fb_betterfollownews_", mod, sep = ""),
      paste("fb_morefakenews_", mod, sep = ""),
      paste("fb_habit_", mod, sep = ""),
      paste("fb_deact_good_", mod, sep = ""),
      paste("fb_posimp_log_", mod, sep = ""),
      paste("fb_negimp_log_", mod, sep = ""),
      paste("index_fbopinions_", mod, sep = "")
    )
  )
  assign(
    paste("vs_postexp_", mod, sep = ""),
    c(
      paste("planned_use_", mod, sep = ""),
      paste("clicked_timelimit_email_", mod, sep = ""),
      paste("time_to_reactivation_", mod, sep = ""),
      paste("mobile_minutes_wins_", mod, sep = ""),
      paste("index_postexp_", mod, sep = "")
    )
  )
}
