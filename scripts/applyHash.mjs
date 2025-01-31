import { copyFile } from 'node:fs/promises';
import crypto from 'crypto';
import fs from 'fs';
import path from 'path';


const publicIndexPath = path.join('public', 'index.html');

const filePaths = [
    ['dist', 'elm.js'],
    ['dist', 'main.css'],
    ['dist', 'src', 'chant_engine.js']
  ]

try {
  await copyFile(path.join('src', 'index.html'), publicIndexPath);
} catch {
  console.error('error copying index.html');
}


function rename(file) {
  const fullPath = path.join('public', ...file);
  const content = fs.readFileSync(fullPath);
  const hash = crypto.createHash('md5').update(content).digest('hex');

  
  const ext = fullPath.split(".").pop();
  const newPath = fullPath.replace(`.${ext}`, `.${hash}.${ext}`);
  
  fs.renameSync(fullPath, newPath);
  console.log(`renamed ${file} to ${newPath}`);

  // update references

  const urlPath = file.join('/');
  const newUrlPath = urlPath.replace(`.${ext}`, `.${hash}.${ext}`);

  const pathRegex = new RegExp(urlPath, 'g');

  let indexContent = fs.readFileSync(publicIndexPath, 'utf8');
  indexContent = indexContent.replace(pathRegex, newUrlPath);
  fs.writeFileSync(publicIndexPath, indexContent, 'utf8');

}

filePaths.forEach(rename);


