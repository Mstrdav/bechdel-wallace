"""
Script pour scraper les films manquant du dataset de Bechdeltest.com, qui s'arrête en 2023.
"""

from bs4 import BeautifulSoup
import requests
import pandas as pd
import tqdm

# pour trouver le dernier film ajouté, on cherche dans notre dataset
df = pd.read_csv('data/raw/movies.csv') # attention chemin relatif, voir ou on lance le script
last_added_movie_date = df['date'].max()
last_added_movie = df[df['date'] == last_added_movie_date]
print(f"Dernier film ajouté : {last_added_movie[["title", "year", "date"]]}")
# 9364 Les choses simples 2023 2023-02-28 10:43:35

url = 'https://bechdeltest.com/sort/added?list=all'
r = requests.get(url)
html = r.content
soup = BeautifulSoup(html, 'html.parser')

# On cherche sur bechdeltest.com tous les films ajoutés depuis le dernier film ajouté (on utilise donc l'endpoint https://bechdeltest.com/sort/added), et l'argument `list=all` pour afficher tous les films.

# query selector: ".list .movie"
movies = soup.select('.list .movie')
print(f"Nombre de films : {len(movies)}")

# On parcourt tous les films, jusqu'à ce que l'on trouve le dernier film ajouté.
to_add = []

for movie in movies:
    # each movie element has tree 'a' children
    # first on is for imdb link, we can extract imdbid
    imdb_link = movie.select('a')[0]
    imdbid = imdb_link['href'].split('/tt')[1].split('/')[0]
    # second one is for movie page link, we can extract movie_id
    movie_link = movie.select('a')[1]
    movie_title = movie_link.text

    if imdbid == str(last_added_movie['imdbid'].values[0]):
        print("Last movie found !")
        break

    to_add.append({
        'imdbid': imdbid,
        'movielink': movie_link['href'],
        'title': movie_title
    })

df_to_add = pd.DataFrame(to_add)
print(df_to_add.info())

# scraping de la page de chaque film à ajouter
# sur les pages de chaque film, on cherche les infos suivantes :
# - year
# - rating
# - dubious
# - date

# progress bar
for movie_link in tqdm.tqdm(df_to_add['movielink'].values): 
    url = 'https://bechdeltest.com' + movie_link
    r = requests.get(url)
    html = r.content
    soup = BeautifulSoup(html, 'html.parser')

    year = soup.select('h2 > a > span')[0].text.split("(")[1].split(")")[0]
    rating = soup.select('h2 + p')[0].text.split("passed ")[1].split(" ")[0]
    dubious = "dubious" in soup.select('h2 + p')[0].text
    date = soup.select('h2 + p > span')[0].text.split("on ")[1].split(".")[0]

    # print(year, rating, dubious, date)

    # on peut maintenant ajouter ces infos au dataset

    df_to_add.loc[df_to_add['movielink'] == movie_link, 'year'] = year
    df_to_add.loc[df_to_add['movielink'] == movie_link, 'rating'] = rating
    df_to_add.loc[df_to_add['movielink'] == movie_link, 'dubious'] = dubious
    df_to_add.loc[df_to_add['movielink'] == movie_link, 'date'] = date

df_to_add.to_csv('data/raw/last_import.csv', index=False)

# concatenation des deux datasets
df = pd.concat([df, df_to_add], ignore_index=True)
df.to_csv('data/raw/all_movies.csv', index=False)
