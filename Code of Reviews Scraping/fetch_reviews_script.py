from selenium import webdriver
from selenium.webdriver.common.keys import Keys
from time import sleep
from selenium.webdriver.common.by import By
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.support.ui import WebDriverWait
import sys
import pandas as pd




all_apps_name = pd.read_csv("upload file",encoding='utf-8')

all_apps_name.drop_duplicates()

reviews_fetched_apps = pd.read_csv("file name")

reviews_fetched_apps.drop_duplicates()

c=all_apps_name.App.isin(reviews_fetched_apps.App).astype(int)
l=0
n=0
o=1001
#Change the number given below for changing the apps. 
apps_name = all_apps_name.iloc[:,]


driver = webdriver.Chrome('path of driver')

reviews_write = pd.DataFrame()

for index,row in apps_name.iterrows():
    try:
        print("app:",row['App'])
        if(c[o]==0):

            
            
            
                
            driver.get("https://play.google.com/store")
            search_box = driver.find_element_by_class_name("gbqfif")
            search_box.send_keys(row['App'])
            search_box_btn = driver.find_element_by_class_name("gbqfb")
            search_box_btn.click()

            wait =WebDriverWait(driver,10)
            elem = wait.until(EC.element_to_be_clickable((By.LINK_TEXT,row['App'])))
            print("Script reached here.")
            app_name = driver.find_element_by_partial_link_text(row['App']).click()


            #print("Script reached here2. ")
            driver.get(driver.current_url+'&showAllReviews=true')


            #full_review_button =  driver.find_elements(By.CLASS_NAME,'LkLjZd.ScJHi.OzU4dc')
            #full_review_button = driver.find_elements(By.PARTIAL_LINK_TEXT,'Full Review')

            #full_review_button = driver.find_elements(By.XPATH,'//*[@id="fcxH9b"]/div[4]/c-wiz/div/div[2]/div/div[1]/div/div/div[1]/div[2]/div/div[7]/div/div[2]/div[2]/span[1]/div/button')

            full_review_button = driver.find_elements(By.CSS_SELECTOR,'button.LkLjZd.ScJHi.OzU4dc')

            #print("Full Review button:",full_review_button)
            for i in full_review_button:
                i.click()


            #reviews = driver.find_element_by_class_name("UD7Dzf")

            non_bmp_map = dict.fromkeys(range(0x10000, sys.maxunicode + 1), 0xfffd)


            reviews = driver.find_elements(By.CLASS_NAME,'UD7Dzf')

            #reviews = driver.find_element_by_xpath("//*[@id='fcxH9b']/div[4]/c-wiz/div/div[2]/div/div[1]/div/div/div[1]/div[2]/div/div[1]/div/div[2]/div[2]")

            #print("Reviews:")
            for i in reviews:
                print("length:",len(reviews))
                print("Reviews:",i.text.translate(non_bmp_map))
                print("n:",n)
                df_temp = pd.DataFrame.from_records({'App':[row['App']],'Review':[i.text.translate(non_bmp_map)]})
                reviews_write = reviews_write.append(df_temp,)
                print("reviews:",reviews_write)
                    
            l+=1
            print("l:",l)
            print("o:",o)
        o+=1
    except:
            l+=1
            print("l:",l)
            o+=1
            print("o:",o)
            continue

reviews_write.to_csv('file name',sep=',',encoding='utf-8',index=False)



#app_name = driver.find_element_by_link_text('10 Best Foods for You').click()
#apps_list = driver.find_element_by_xpath("//*[@id='body-content']/div[2]/div/div[1]/div/div[4]/div/div[1]/h2/a")
##apps_list.click()
#apps_list= driver.find_element_by_css_selector('a.title-link.id-track-click').click()




'''
#driver.execute_script("document.getElementsByClassName('icon-container')[1].click()")

wait =WebDriverWait(driver,50)
elem = wait.until(EC.element_to_be_clickable((By.CLASS_NAME,'icon-container'))).click()
print("Script")


#driver.find_element_by_class_name('icon-container').click()
#app=driver.find_element_by_css_selector('a.title').click()

#link="https://play.google.com/store/search?q="
#link2="&hl=en"
'''
