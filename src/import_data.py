"""
Script pour scraper les films manquant du dataset de Bechdeltest.com, qui s'arrête en 2023.
"""

from bs4 import BeautifulSoup
import requests
import pandas as pd

url = 'https://bechdeltest.com/'
r = requests.get(url)
html = r.content
soup = BeautifulSoup(html, 'html.parser')

# pour trouver le dernier film ajouté, on cherche dans notre dataset
df = pd.read_csv('data/raw/movies.csv')
last_added_movie = df['date'].max()
print(f"Dernier film ajouté : {last_added_movie}")
