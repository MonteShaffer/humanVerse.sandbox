#!/usr/bin/env python -u
import argparse
import sys
import os
import time
import datetime
import random  
import base64


file_name = "Line Combinations - Frozen Tools.csv"
download_path = "C:/Users/Alexander Nevsky/Downloads/"


start_time = str(time.time())

def getOptions(args=sys.argv[1:]):
    parser = argparse.ArgumentParser(description="Parses command.")
    parser.add_argument("-t", "--team", help="NHL team")
    parser.add_argument("-r", "--remote", help="Remote url")
    parser.add_argument("-l", "--local",  help="Local Path")
    parser.add_argument("-o", "--open", help="Open Browser Headless")
    parser.add_argument("-s", "--sleep", type=int, help="Milliseconds to wait for DOM to settle.")
    parser.add_argument("-v", "--verbose",dest='verbose',action='store_true', help="Verbose mode.")
    options = parser.parse_args(args)
    return options
    
options = getOptions()    
#print(options)


# https://stackabuse.com/encoding-and-decoding-base64-strings-in-python/
def base64_decode(b64_string):
    base64_bytes = b64_string.encode('ascii')
    message_bytes = base64.b64decode(base64_bytes)
    message = message_bytes.decode('ascii')
    return message

options.remote = base64_decode(options.remote)

#print(options.remote)


wait_seconds = 8
sleep_factor_min = 3
sleep_factor_max = 5

def randomSleep():
    #mysleepr = random.randint(sleep_factor_min,sleep_factor_max)
    mysleepr = random.uniform(sleep_factor_min,sleep_factor_max)
    mysleep = mysleepr * options.sleep / 1000
    time.sleep(mysleep)

page_csv = options.local + options.team + ".csv"
page_html = options.local + options.team + ".html"

#print(page_csv)
#quit()

# if it is cached, let's quit 
if os.path.isfile(page_csv):
    print("\n cached \n")
    quit()
    

from selenium import webdriver 
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC

chrome_options = Options()
chrome_options.add_argument("user-agent=[Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/536.5 (KHTML, like Gecko) Chrome/19.0.1084.56 Safari/536.5]")
#chrome_options.add_argument("user-agent=[Mozilla/5.0 (iPad; CPU OS 9_3_5 like Mac OS X) AppleWebKit/601.1.46 (KHTML, like Gecko) Version/9.0 Mobile/13G36 Safari/601.1]")
# chrome_options.add_argument("--disable-extensions")  
chrome_options.add_argument("window-size=1920,1080")  

# https://stackoverflow.com/questions/18026391/selenium-webdriver-in-python-files-download-directory-change-in-chrome-prefere
#prefs = {"download.default_directory" : options.local}
#chrome_options.add_experimental_option("prefs", prefs)  

# driver.set_window_size(1920, 1080)
chrome_options.add_argument("--disable-gpu")
chrome_options.add_argument("--verbose")

# https://stackoverflow.com/questions/27824124/how-to-change-file-download-location-in-webdriver-while-using-chrome-driver-fire
#chrome_options.add_argument("--no-sandbox") # linux only

if options.open != "true":
    chrome_options.add_argument("--headless") # if headless, I need a window size ...
    # chrome_options.headless = True # also works
    
# C:/python3/python.exe C:/_git_/github/MonteShaffer/humanVerse/misc/NHL/get.csv.py --team=WSH --remote=aHR0cHM6Ly9mcm96ZW5wb29sLmRvYmJlcnNwb3J0cy5jb20vZnJvemVucG9vbF9saW5lY29tYm8ucGhwP3NlbGVjdD1URiZ0ZWFtZj1XU0gmZ2FtZXM9MjAyMC0yMDIxJTNBUiUzQTk5JnBlcmlvZD1BTEwmc2l0dWF0aW9uPUVW --local=R:/project-NHL/2021-16/ --sleep=250 --open=true

# downloaded from chromium.org, version 89
# chromedriver.chromium.org/downloads
chrome_options.add_argument("driver_path='C:/chromedriver/chromedriver.exe'")
driver = webdriver.Chrome(options=chrome_options)
# driver = webdriver.Chrome(options=chrome_options, executable_path='C:/chromedriver/chromedriver.exe') 
# driver.set_window_size(1920, 1080)
# https://stackoverflow.com/questions/64717302/deprecationwarning-executable-path-has-been-deprecated-selenium-python
# https://www.programcreek.com/python/example/100025/selenium.webdriver.ChromeOptions

from selenium.common.exceptions import NoSuchElementException
def check_exists_by_id(id):
    try:
        driver.find_element_by_id(id)
    except NoSuchElementException:
        return False
    return True
    
#from selenium.common.exceptions import StaleElementReferenceException

driver.get(options.remote)

randomSleep()


randomSleep()
print(driver.execute_script("return document.title;"))
randomSleep()

html = str(driver.page_source.encode("utf-8"))
if os.path.exists(page_html):
    print("\n\n page exists -- \n\n")
else:
    f = open(page_html, 'w', encoding="utf-8")
    f.write(html)
    f.close()

    
def wait_located(dr, x):
    try:
        element = WebDriverWait(dr, wait_seconds).until(
            EC.presence_of_element_located((By.XPATH, x))
            )
        return element
    except:
        return False
    
def wait_and_click(dr, x):
    try:
        element = WebDriverWait(dr, wait_seconds).until(
            EC.element_to_be_clickable((By.XPATH, x))
            ).click()
        return True
    except:
        return False
    
# http://blog.likewise.org/2015/04/scrolling-to-an-element-with-the-python-bindings-for-selenium-webdriver/
def scroll_to(dr, x):
    try:
        element = WebDriverWait(dr, wait_seconds).until(
            EC.presence_of_element_located((By.XPATH, x))
            ).execute_script("return arguments[0].scrollIntoView();", element)
        return True
    except:
        return False



# <span>Download CSV File</span>
# //span[contains(@class, 'off') and text() = 'Septuagint']
# driver.find_elements_by_xpath("//*[contains(text(), 'My Button')]")
# https://stackoverflow.com/questions/12323403/how-do-i-find-an-element-that-contains-specific-text-in-selenium-webdriver-pyth
# xpath = "//span[contains(text() = 'Download CSV File'])"
# xpath = "//*[contains(text(), 'Download CSV File')]"
# https://stackoverflow.com/questions/55742514/how-to-find-element-by-span-without-class-or-title-in-selenium
xpath = "//button[contains(@class, 'buttons-csv')]"
wait_located(driver, xpath)   
randomSleep()
# container panel-rounded-top footer
# randomSleep()
# randomSleep()
# randomSleep()
# scroll_to(driver, "//div[contains(@class, 'footer')]")
# scroll_to(drive, xpath)
driver.execute_script("window.scrollTo(0, document.body.scrollHeight)")

randomSleep()
if wait_and_click(driver, xpath):
    print("download csv \n\n")
    randomSleep()
    # save      
else:       
    print("NOT FOUND ... download csv \n\n")
    
    
# firefoxProfile.setPreference("browser.helperApps.neverAsk.saveToDisk",                      "application/octet-stream,text/csv");  


os.rename( download_path + file_name, page_csv);



      
end_time = str(time.time())    
total_time = str( float(end_time) - float(start_time) )
str_time = start_time + "\n" + end_time + "\n" + total_time

print(str_time)

driver.quit()        
quit()


# stackoverflow.com/questions/52859981/selenium-generating-error-element-is-not-interactable