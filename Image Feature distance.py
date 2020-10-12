#남자 캐릭터 배우 이미지유사도


import cv2
import numpy as np
import matplotlib.pyplot as plt
import os
os.chdir("C:\\Users\\love2\\Desktop\\사진\\men_women\\men\\m_face\\m_img_m400")
from glob import glob
features = []
labels = []
images = glob('*.jpg')
cha=0
for im in images:
    image = cv2.imread(im,cv2.IMREAD_GRAYSCALE)
    orb = cv2.ORB_create(nfeatures=100)
    kp1,des1 = orb.detectAndCompute(image,None)
    features.append(des1.ravel())
features = np.array(features)

#################################
#caculate distance

from sklearn.preprocessing import StandardScaler
sc = StandardScaler()
features = sc.fit_transform(features)
from scipy.spatial import distance
dists = distance.squareform(distance.pdist(features))


#################################
#search actor def

def selectImage(n, m, dists, images):
    act_len = 111
    if m == 0:
        image = cv2.imread(images[n+act_len])
        image = cv2.cvtColor(image, cv2.COLOR_BGR2RGB)
        return image 
    else:
        target_image = images[n+act_len]
        if len(target_image)<10:
            act_num = target_image[-6:-4]
            if act_num[0]=='0':
                act_num = act_num[1]
        elif len(target_image)==10:
            act_num = target_image[-7:-4]
        else:
            act_num = target_image[-7:-4]
            if act_num[0]=='0':
                if act_num[1]=='0':
                    act_num = act_num[2]
                else:
                    act_num = act_num[1:]
            
        
        n_dists_rank = dists[n+act_len].argsort()
        a_dists = [0]
        for i in n_dists_rank:
            if i<act_len:
                a_dists.append(i)
        image_position = a_dists[m]
    

        if int(act_num)<10:
            f_name = 'man0'+act_num+'.jpg'
        else:
            f_name = 'man'+act_num+'.jpg'
        if (images[image_position]==f_name):
            print(act_num,' ',m)
        image = cv2.imread(images[image_position])
        image = cv2.cvtColor(image, cv2.COLOR_BGR2RGB)
        return image

    


#################################
#actor visualization def

def plotImages(n):
    plt.figure(figsize=(15,5))

    plt.subplot(141)
    plt.imshow(selectImage(n,0, dists, images))
    plt.title('Original')
    plt.xticks([])
    plt.yticks([])

    plt.subplot(142)
    plt.imshow(selectImage(n,1, dists, images))
    plt.title('1st simular one')
    plt.xticks([])
    plt.yticks([])

    plt.subplot(143)
    plt.imshow(selectImage(n,2, dists, images))
    plt.title('2nd simular one')
    plt.xticks([])
    plt.yticks([])

    plt.subplot(144)
    plt.imshow(selectImage(n,3, dists, images))
    plt.title('3rd simular one')
    plt.xticks([])
    plt.yticks([])

    plt.show()
    
    
#########################################
#matching code

a=0
for j in range(0,67):
    a+=1
    for i in range(1,112):
        selectImage(j, i, dists, images)
##########################################3
#visualization code

plotImages(6)
