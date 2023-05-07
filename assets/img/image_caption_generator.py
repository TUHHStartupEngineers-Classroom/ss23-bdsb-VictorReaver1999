import os

# Get a list of all image files in the "images" folder
image_files = [f for f in os.listdir("sales_year_state") if f.endswith(".png")]

# Open a new file for writing
with open("sales_year_state.txt", "w") as f:
    # Loop through the image files and generate an image tag for each one
    for i in image_files:
        f.write(f"![Caption](../../assets/img/sales_year_state/{i})\n\n")
