## mine poems from haiku forth salon ##

# import

from pyquery import PyQuery as pq
import requests
import re

# constants

haiku_base = "http://forthsalon.appspot.com"
haiku_list = haiku_base + "/haiku-list?order=score"

ua = 'Mozilla/5.0 (Windows NT 6.3; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/37.0.2049.0 Safari/537.36'

# functions

def mine_haikus(url = haiku_list, f = None):
    """mine all haikus, starting from top haikus page"""
    print("mining: " + str(url))
    r = requests.get(url, headers = {'User-Agent' : ua})
    d = pq(r.text)

    haikus = []
    for haiku in d("div.section")("div.haiku"):
        title  = haiku.find("b").text_content()
        author = haiku.find("i").text_content()
        code   = haiku.find("span").find("textarea").text_content()

        if f is not None:
            f.write("### " + title + "\n")
            f.write("### " + author + "\n\n")
            f.write(code + "\n\n")
            f.flush()

    a = d("h3")("a")
    if (a):
        return haikus + mine_haikus(url = haiku_base + a[0].attrib['href'], f = f)
    else:
        return haikus

if __name__ == "__main__":
    f = open('haikus.txt', 'w', encoding = 'utf-8')
    mine_haikus(url = haiku_list, f = f)
    f.close()
    pass

# ['mining: http://forthsalon.appspot.com/haiku-list?order=score', 'E-ABAOsB8gEFc2NvcmX6AQIIB-wBggIhahBzfmZvcnRoc2Fsb24taHJkcg0LEgVIYWlrdRiH-gEMiAIBFA==',
# 'E-ABAOsB8gEFc2NvcmX6AQIIBewBggImahBzfmZvcnRoc2Fsb24taHJkchILEgVIYWlrdRiAgICAo52dCwyIAgEU', 'E-ABAOsB8gEFc2NvcmX6AQIIA-wBggIhahBzfmZvcnRoc2Fsb24taHJkcg0LEgVIYWlrdRjimxYMiAIBFA==',
# 'E-ABAOsB8gEFc2NvcmX6AQIIA-wBggImahBzfmZvcnRoc2Fsb24taHJkchILEgVIYWlrdRiAgICAgOSRCwyIAgEU', 'E-ABAOsB8gEFc2NvcmX6AQIIAuwBggIhahBzfmZvcnRoc2Fsb24taHJkcg0LEgVIYWlrdRjH_xQMiAIBFA==',
# 'E-ABAOsB8gEFc2NvcmX6AQIIAuwBggImahBzfmZvcnRoc2Fsb24taHJkchILEgVIYWlrdRiAgIDAuMuCCgyIAgEU', 'E-ABAOsB8gEFc2NvcmX6AQIIAuwBggImahBzfmZvcnRoc2Fsb24taHJkchILEgVIYWlrdRiAgICA0eKMCwyIAgEU',
# 'E-ABAOsB8gEFc2NvcmX6AQIIAewBggIhahBzfmZvcnRoc2Fsb24taHJkcg0LEgVIYWlrdRj5zAMMiAIBFA==', 'E-ABAOsB8gEFc2NvcmX6AQIIAewBggImahBzfmZvcnRoc2Fsb24taHJkchILEgVIYWlrdRiAgICA4LDFCAyIAgEU',
# 'E-ABAOsB8gEFc2NvcmX6AQIIAewBggImahBzfmZvcnRoc2Fsb24taHJkchILEgVIYWlrdRiAgICAgOSxCQyIAgEU', 'E-ABAOsB8gEFc2NvcmX6AQIIAewBggImahBzfmZvcnRoc2Fsb24taHJkchILEgVIYWlrdRiAgICAkMmPCgyIAgEU',
# 'E-ABAOsB8gEFc2NvcmX6AQIIAewBggImahBzfmZvcnRoc2Fsb24taHJkchILEgVIYWlrdRiAgICA4_TKCgyIAgEU', 'E-ABAOsB8gEFc2NvcmX6AQIIAOwBggIgahBzfmZvcnRoc2Fsb24taHJkcgwLEgVIYWlrdRiiHwyIAgEU',
# 'E-ABAOsB8gEFc2NvcmX6AQIIAOwBggIhahBzfmZvcnRoc2Fsb24taHJkcg0LEgVIYWlrdRiMpAEMiAIBFA==', 'E-ABAOsB8gEFc2NvcmX6AQIIAOwBggIhahBzfmZvcnRoc2Fsb24taHJkcg0LEgVIYWlrdRiSyAIMiAIBFA==',
# 'E-ABAOsB8gEFc2NvcmX6AQIIAOwBggIhahBzfmZvcnRoc2Fsb24taHJkcg0LEgVIYWlrdRiRwgQMiAIBFA==', 'E-ABAOsB8gEFc2NvcmX6AQIIAOwBggIhahBzfmZvcnRoc2Fsb24taHJkcg0LEgVIYWlrdRie4wYMiAIBFA==',
# 'E-ABAOsB8gEFc2NvcmX6AQIIAOwBggIhahBzfmZvcnRoc2Fsb24taHJkcg0LEgVIYWlrdRjykwkMiAIBFA==', 'E-ABAOsB8gEFc2NvcmX6AQIIAOwBggIhahBzfmZvcnRoc2Fsb24taHJkcg0LEgVIYWlrdRii1BgMiAIBFA==',
# 'E-ABAOsB8gEFc2NvcmX6AQIIAOwBggImahBzfmZvcnRoc2Fsb24taHJkchILEgVIYWlrdRiAgICAwPqzCAyIAgEU', 'E-ABAOsB8gEFc2NvcmX6AQIIAOwBggImahBzfmZvcnRoc2Fsb24taHJkchILEgVIYWlrdRiAgICAoI6BCQyIAgEU',
# 'E-ABAOsB8gEFc2NvcmX6AQIIAOwBggImahBzfmZvcnRoc2Fsb24taHJkchILEgVIYWlrdRiAgICA0KWVCQyIAgEU', 'E-ABAOsB8gEFc2NvcmX6AQIIAOwBggImahBzfmZvcnRoc2Fsb24taHJkchILEgVIYWlrdRiAgICAoI7BCQyIAgEU',
# 'E-ABAOsB8gEFc2NvcmX6AQIIAOwBggImahBzfmZvcnRoc2Fsb24taHJkchILEgVIYWlrdRiAgICAx-GCCgyIAgEU', 'E-ABAOsB8gEFc2NvcmX6AQIIAOwBggImahBzfmZvcnRoc2Fsb24taHJkchILEgVIYWlrdRiAgICAhLSKCgyIAgEU',
# 'E-ABAOsB8gEFc2NvcmX6AQIIAOwBggImahBzfmZvcnRoc2Fsb24taHJkchILEgVIYWlrdRiAgICAhKCTCgyIAgEU', 'E-ABAOsB8gEFc2NvcmX6AQIIAOwBggImahBzfmZvcnRoc2Fsb24taHJkchILEgVIYWlrdRiAgICAiKacCgyIAgEU',
# 'E-ABAOsB8gEFc2NvcmX6AQIIAOwBggImahBzfmZvcnRoc2Fsb24taHJkchILEgVIYWlrdRiAgICAwOHKCgyIAgEU', 'E-ABAOsB8gEFc2NvcmX6AQIIAOwBggImahBzfmZvcnRoc2Fsb24taHJkchILEgVIYWlrdRiAgICAiLyNCwyIAgEU',
# 'E-ABAOsB8gEFc2NvcmX6AQIIAOwBggImahBzfmZvcnRoc2Fsb24taHJkchILEgVIYWlrdRiAgICAoP3JCwyIAgEU', 'E-ABAOsB8gEFc2NvcmX6AQsI____________AewBggImahBzfmZvcnRoc2Fsb24taHJkchILEgVIYWlrdRiAgICA1LrGCQyIAgEU']
