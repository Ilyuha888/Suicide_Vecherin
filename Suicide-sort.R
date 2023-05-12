library(tidyverse)

"%+%" <- function(...){
  paste0(...)
}

#Считываем столбцы с вопросами и ответами, удаляем лишние строки, оставшиеся после экселя
base <- drop_na(read_csv2('rep-base.csv'))

#Добавляем разметку вопросов
#Алгоритм проходится по каждому файлу с разметкой
for (x in 1:12) {
  #Тут имя складывает и считывает
  name <- 'rep-'%+%as.character(x)%+%'.csv'
  portion <- read_csv2(name)
  #Удаляем лишние столбцы от экселя
  portion %>% select('rep':'r33-') -> portion
  #Опускаем пустые строки, которые ребята не смогли оценить
  portion %>% drop_na() -> portion
  #Переименовываем столбцы, чтобы показывали номер оценщика
  rename_with(
    portion,
    ~ paste0(.x, as.character(x), recycle0 = TRUE),
    ends_with("-")
  ) -> portion 
  #Приклеиваем к старой таблице
  base %>% left_join(portion) -> base
  #Удаляем случайные дубли
  base %>% distinct(rep, answer, .keep_all = TRUE) -> base
}

#Аналогично только для разметки ответов
for (x in 1:9) {
  name <- 'answ-'%+%as.character(x)%+%'.csv'
  portion <- read_csv2(name)
  portion %>% select('answer':'a31-') -> portion
  portion %>% drop_na() -> portion
  rename_with(
    portion,
    ~ paste0(.x, as.character(x), recycle0 = TRUE),
    ends_with("-")
  ) -> portion 
  base %>% left_join(portion) -> base
  base %>% distinct(rep, answer, .keep_all = TRUE) -> base
}

#Избавляемся от Экселевских косяков...
rename_all(base, funs(str_replace_all(., "-", "_"))) -> base
rename_all(base, funs(str_replace_all(., "q", "a"))) -> base


#Убираем лишние charы
base %>% rowid_to_column("ID") -> base
base %>% select(ID, rep, answer) -> base_1
base %>% select(ID, r1_1:a31_9) %>% mutate_if(is.character, as.numeric) -> base_2
left_join(base_1,base_2) %>% select(-ID) -> base

#Посчитаем средние балы
#Можно было изящнее, но я уже 8 часов сижу, поэтому так

base %>% rowwise() %>% mutate(
  r1 = mean(c(r1_1, r1_2, r1_3, r1_4,r1_5, r1_6, r1_7, r1_8,r1_9, r1_10, r1_11, r1_12), na.rm = TRUE),
  r2 = mean(c(r2_1, r2_2, r2_3, r2_4,r2_5, r2_6, r2_7, r2_8,r2_9, r2_10, r2_11, r2_12), na.rm = TRUE),
  r3 = mean(c(r3_1, r3_2, r3_3, r3_4,r3_5, r3_6, r3_7, r3_8,r3_9, r3_10, r3_11, r3_12), na.rm = TRUE),
  r4 = mean(c(r4_1, r4_2, r4_3, r4_4,r4_5, r4_6, r4_7, r4_8,r4_9, r4_10, r4_11, r4_12), na.rm = TRUE),
  r5 = mean(c(r5_1, r5_2, r5_3, r5_4,r5_5, r5_6, r5_7, r5_8,r5_9, r5_10, r5_11, r5_12), na.rm = TRUE),
  r6 = mean(c(r6_1, r6_2, r6_3, r6_4,r6_5, r6_6, r6_7, r6_8,r6_9, r6_10, r6_11, r6_12), na.rm = TRUE),
  r7 = mean(c(r7_1, r7_2, r7_3, r7_4,r7_5, r7_6, r7_7, r7_8,r7_9, r7_10, r7_11, r7_12), na.rm = TRUE),
  r8 = mean(c(r8_1, r8_2, r8_3, r8_4,r8_5, r8_6, r8_7, r8_8,r8_9, r8_10, r8_11, r8_12), na.rm = TRUE),
  r9 = mean(c(r9_1, r9_2, r9_3, r9_4,r9_5, r9_6, r9_7, r9_8,r9_9, r9_10, r9_11, r9_12), na.rm = TRUE),
  r10 = mean(c(r10_1, r10_2, r10_3, r10_4,r10_5, r10_6, r10_7, r10_8,r10_9, r10_10, r10_11, r10_12), na.rm = TRUE),
  r11 = mean(c(r11_1, r11_2, r11_3, r11_4,r11_5, r11_6, r11_7, r11_8,r11_9, r11_10, r11_11, r11_12), na.rm = TRUE),
  r12 = mean(c(r12_1, r12_2, r12_3, r12_4,r12_5, r12_6, r12_7, r12_8,r12_9, r12_10, r12_11, r12_12), na.rm = TRUE),
  r13 = mean(c(r13_1, r13_2, r13_3, r13_4,r13_5, r13_6, r13_7, r13_8,r13_9, r13_10, r13_11, r13_12), na.rm = TRUE),
  r14 = mean(c(r14_1, r14_2, r14_3, r14_4,r14_5, r14_6, r14_7, r14_8,r14_9, r14_10, r14_11, r14_12), na.rm = TRUE),
  r15 = mean(c(r15_1, r15_2, r15_3, r15_4,r15_5, r15_6, r15_7, r15_8,r15_9, r15_10, r15_11, r15_12), na.rm = TRUE),
  r16 = mean(c(r16_1, r16_2, r16_3, r16_4,r16_5, r16_6, r16_7, r16_8,r16_9, r16_10, r16_11, r16_12), na.rm = TRUE),
  r17 = mean(c(r17_1, r17_2, r17_3, r17_4,r17_5, r17_6, r17_7, r17_8,r17_9, r17_10, r17_11, r17_12), na.rm = TRUE),
  r18 = mean(c(r18_1, r18_2, r18_3, r18_4,r18_5, r18_6, r18_7, r18_8,r18_9, r18_10, r18_11, r18_12), na.rm = TRUE),
  r19 = mean(c(r19_1, r19_2, r19_3, r19_4,r19_5, r19_6, r19_7, r19_8,r19_9, r19_10, r19_11, r19_12), na.rm = TRUE),
  r20 = mean(c(r20_1, r20_2, r20_3, r20_4,r20_5, r20_6, r20_7, r20_8,r20_9, r20_10, r20_11, r20_12), na.rm = TRUE),
  r21 = mean(c(r21_1, r21_2, r21_3, r21_4,r21_5, r21_6, r21_7, r21_8,r21_9, r21_10, r21_11, r21_12), na.rm = TRUE),
  r22 = mean(c(r22_1, r22_2, r22_3, r22_4,r22_5, r22_6, r22_7, r22_8,r22_9, r22_10, r22_11, r22_12), na.rm = TRUE),
  r23 = mean(c(r23_1, r23_2, r23_3, r23_4,r23_5, r23_6, r23_7, r23_8,r23_9, r23_10, r23_11, r23_12), na.rm = TRUE),
  r24 = mean(c(r24_1, r24_2, r24_3, r24_4,r24_5, r24_6, r24_7, r24_8,r24_9, r24_10, r24_11, r24_12), na.rm = TRUE),
  r25 = mean(c(r25_1, r25_2, r25_3, r25_4,r25_5, r25_6, r25_7, r25_8,r25_9, r25_10, r25_11, r25_12), na.rm = TRUE),
  r26 = mean(c(r26_1, r26_2, r26_3, r26_4,r26_5, r26_6, r26_7, r26_8,r26_9, r26_10, r26_11, r26_12), na.rm = TRUE),
  r27 = mean(c(r27_1, r27_2, r27_3, r27_4,r27_5, r27_6, r27_7, r27_8,r27_9, r27_10, r27_11, r27_12), na.rm = TRUE),
  r28 = mean(c(r28_1, r28_2, r28_3, r28_4,r28_5, r28_6, r28_7, r28_8,r28_9, r28_10, r28_11, r28_12), na.rm = TRUE),
  r29 = mean(c(r29_1, r29_2, r29_3, r29_4,r29_5, r29_6, r29_7, r29_8,r29_9, r29_10, r29_11, r29_12), na.rm = TRUE),
  r30 = mean(c(r30_1, r30_2, r30_3, r30_4,r30_5, r30_6, r30_7, r30_8,r30_9, r30_10, r30_11, r30_12), na.rm = TRUE),
  r31 = mean(c(r31_1, r31_2, r31_3, r31_4,r31_5, r31_6, r31_7, r31_8,r31_9, r31_10, r31_11, r31_12), na.rm = TRUE),
  r32 = mean(c(r32_1, r32_2, r32_3, r32_4,r32_5, r32_6, r32_7, r32_8,r32_9, r32_10, r32_11, r32_12), na.rm = TRUE),
  r33 = mean(c(r33_1, r33_2, r33_3, r33_4,r33_5, r33_6, r33_7, r33_8,r33_9, r33_10, r33_11, r33_12), na.rm = TRUE)
) ->  base

base %>% rowwise() %>% mutate(
  a1 = mean(c(a1_1, a1_2, a1_3, a1_4,a1_5, a1_6, a1_7, a1_8,a1_9), na.rm = TRUE),
  a2 = mean(c(a2_1, a2_2, a2_3, a2_4,a2_5, a2_6, a2_7, a2_8,a2_9), na.rm = TRUE),
  a3 = mean(c(a3_1, a3_2, a3_3, a3_4,a3_5, a3_6, a3_7, a3_8,a3_9), na.rm = TRUE),
  a4 = mean(c(a4_1, a4_2, a4_3, a4_4,a4_5, a4_6, a4_7, a4_8,a4_9), na.rm = TRUE),
  a5 = mean(c(a5_1, a5_2, a5_3, a5_4,a5_5, a5_6, a5_7, a5_8,a5_9), na.rm = TRUE),
  a6 = mean(c(a6_1, a6_2, a6_3, a6_4,a6_5, a6_6, a6_7, a6_8,a6_9), na.rm = TRUE),
  a7 = mean(c(a7_1, a7_2, a7_3, a7_4,a7_5, a7_6, a7_7, a7_8,a7_9), na.rm = TRUE),
  a8 = mean(c(a8_1, a8_2, a8_3, a8_4,a8_5, a8_6, a8_7, a8_8,a8_9), na.rm = TRUE),
  a9 = mean(c(a9_1, a9_2, a9_3, a9_4,a9_5, a9_6, a9_7, a9_8,a9_9), na.rm = TRUE),
  a10 = mean(c(a10_1, a10_2, a10_3, a10_4,a10_5, a10_6, a10_7, a10_8,a10_9), na.rm = TRUE),
  a11 = mean(c(a11_1, a11_2, a11_3, a11_4,a11_5, a11_6, a11_7, a11_8,a11_9), na.rm = TRUE),
  a12 = mean(c(a12_1, a12_2, a12_3, a12_4,a12_5, a12_6, a12_7, a12_8,a12_9), na.rm = TRUE),
  a13 = mean(c(a13_1, a13_2, a13_3, a13_4,a13_5, a13_6, a13_7, a13_8,a13_9), na.rm = TRUE),
  a14 = mean(c(a14_1, a14_2, a14_3, a14_4,a14_5, a14_6, a14_7, a14_8,a14_9), na.rm = TRUE),
  a15 = mean(c(a15_1, a15_2, a15_3, a15_4,a15_5, a15_6, a15_7, a15_8,a15_9), na.rm = TRUE),
  a16 = mean(c(a16_1, a16_2, a16_3, a16_4,a16_5, a16_6, a16_7, a16_8,a16_9), na.rm = TRUE),
  a17 = mean(c(a17_1, a17_2, a17_3, a17_4,a17_5, a17_6, a17_7, a17_8,a17_9), na.rm = TRUE),
  a18 = mean(c(a18_1, a18_2, a18_3, a18_4,a18_5, a18_6, a18_7, a18_8,a18_9), na.rm = TRUE),
  a19 = mean(c(a19_1, a19_2, a19_3, a19_4,a19_5, a19_6, a19_7, a19_8,a19_9), na.rm = TRUE),
  a20 = mean(c(a20_1, a20_2, a20_3, a20_4,a20_5, a20_6, a20_7, a20_8,a20_9), na.rm = TRUE),
  a21 = mean(c(a21_1, a21_2, a21_3, a21_4,a21_5, a21_6, a21_7, a21_8,a21_9), na.rm = TRUE),
  a22 = mean(c(a22_1, a22_2, a22_3, a22_4,a22_5, a22_6, a22_7, a22_8,a22_9), na.rm = TRUE),
  a23 = mean(c(a23_1, a23_2, a23_3, a23_4,a23_5, a23_6, a23_7, a23_8,a23_9), na.rm = TRUE),
  a24 = mean(c(a24_1, a24_2, a24_3, a24_4,a24_5, a24_6, a24_7, a24_8,a24_9), na.rm = TRUE),
  a25 = mean(c(a25_1, a25_2, a25_3, a25_4,a25_5, a25_6, a25_7, a25_8,a25_9), na.rm = TRUE),
  a26 = mean(c(a26_1, a26_2, a26_3, a26_4,a26_5, a26_6, a26_7, a26_8,a26_9), na.rm = TRUE),
  a27 = mean(c(a27_1, a27_2, a27_3, a27_4,a27_5, a27_6, a27_7, a27_8,a27_9), na.rm = TRUE),
  a28 = mean(c(a28_1, a28_2, a28_3, a28_4,a28_5, a28_6, a28_7, a28_8,a28_9), na.rm = TRUE),
  a29 = mean(c(a29_1, a29_2, a29_3, a29_4,a29_5, a29_6, a29_7, a29_8,a29_9), na.rm = TRUE),
  a30 = mean(c(a30_1, a30_2, a30_3, a30_4,a30_5, a30_6, a30_7, a30_8,a30_9), na.rm = TRUE),
  a31 = mean(c(a31_1, a31_2, a31_3, a31_4,a31_5, a31_6, a31_7, a31_8,a31_9), na.rm = TRUE)
) ->  base

#Посчитаем суммы по запросам (r) и ответам (a)
base %>% rowwise() %>% mutate(
  sum_r = sum(r1,	r2,	r3,	r4,	r5,	r6,	r7,	r8,	r9,	r10,	r11,	r12,	r13,	r14,	r15,	r16,	r17,	r18,	r19,	r20,	r21,	r22,	r23,	r24,	r25,	r26,	r27,	r28,	r29,	r30,	r31,	r32,	r33),
  sum_a = sum(a1,	a2,	a3,	a4,	a5,	a6,	a7,	a8,	a9,	a10,	a11,	a12,	a13,	a14,	a15,	a16,	a17,	a18,	a19,	a20,	a21,	a22,	a23,	a24,	a25,	a26,	a27,	a28,	a29,	a30,	a31)
) ->  base

base %>% write.csv('itog_full.csv')
#Тут вопросы, ответы, средние баллы и итоговые суммы
base %>% select(rep, answer, r1:sum_a) -> base_short

base_short %>% write.csv('itog_short.csv')
#Тут вопросы, ответы, итоговая сумма реплик, отсортировано от самых крутых и вниз
base_short %>% select(rep, answer, sum_a) %>% arrange(desc(sum_a)) -> sort_otv

sort_otv %>% write.csv('sort_otv.csv')
