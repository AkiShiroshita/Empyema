chart_clean <- function(chart){
  chart <<- chart %>% 
    rename(adm_date = "入院年月日",
           dev_place = "発症場所",
           diag_date = "膿胸診断日（推定）",
           last_date = "最終安否確認日",
           last_condition = "最終安否",
           fever = "発熱",
           cough = "咳嗽",
           sputum = "喀痰",
           chest_pain = "胸痛",
           weight_loss = "体重減少",
           surgery_3m = "入院前3ヶ月以内の外科手術",
           damage_3m = "入院前3ヶ月以内の胸部外傷歴",
           hot = "在宅酸素",
           hot_ryou_ansei = "在宅酸素安静時流量",
           hot_ryou_rousa = "在宅酸素労作時流量",
           pleural_look = "胸水肉眼所見",
           sbp = "診断日収縮期血圧",
           dbp = "診断日拡張期血圧",
           hr = "診断日脈拍",
           rr = "診断日呼吸数",
           spo2 = "診断日Spo2",
           o2 = "診断日酸素投与量") %>% 
    select(-3, -4) %>% 
    mutate(adm_date = ymd(adm_date),
           diag_date= ymd(diag_date)) %>% 
    mutate(dev_place = case_when(dev_place == "市中発症" ~ 0,
                                 dev_place == "院内発症" ~ 1,
                                 dev_place == "不明" ~ 2),
           last_condition = if_else(last_condition == "生存", 0, 1),
           fever = if_else(fever == "なし", 0, 1),
           cough = if_else(cough == "なし", 0, 1),
           sputum = if_else(sputum == "なし", 0, 1),
           chest_pain = if_else(chest_pain == "なし", 0, 1),
           weight_loss = if_else(weight_loss == "なし", 0, 1),
           surgery_3m = if_else(surgery_3m == "なし", 0, 1),
           damage_3m = if_else(damage_3m == "なし", 0, 1),
           hot = if_else(hot == "なし", 0, 1),
           pleural_look = case_when(pleural_look == "膿性ではない" ~ "0",
                                    pleural_look == "膿性" ~ "1",
                                    pleural_look == "不明" ~ "2")) %>% 
    mutate_all(.funs = ~ as.character(.)) 
}
